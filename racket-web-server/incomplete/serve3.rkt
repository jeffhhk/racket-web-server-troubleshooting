#lang racket
#|
    An attempt WIP to follow recommendations on response logging from Jay McCarthy and Greg Hendershott:
        https://github.com/racket/web-server/issues/54
        https://stackoverflow.com/questions/53911162/how-to-show-http-status-code-in-web-server-logs
|#

(require
        web-server/web-server
        web-server/http
        web-server/http/response
        web-server/private/connection-manager
         (prefix-in limit: web-server/dispatchers/limit)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)        
         web-server/servlet
         web-server/servlet-env
         json)
         
(define eol-bytes (string->bytes/utf-8 "\n"))

(define ((log-fn thunk) . args)
  (define t0 (current-milliseconds))
  (dynamic-wind
    (lambda () '())
    (lambda () (apply thunk args))
    (lambda () 
      (define dt (- (current-milliseconds) t0))
      (printf "finished in ~ams\n" dt))))

(define (long-response op)
  (for ((i (range 0 5000000)))
                  (write-bytes (jsexpr->bytes `#hasheq((i . ,i))) op)
                  (write-bytes eol-bytes op)))

(define (my-app req)
  (define t0 (current-milliseconds))
  (response
    200 #"OK"
    t0 TEXT/HTML-MIME-TYPE
    empty
    (log-fn long-response)))

(define (headers->hasheq hs)
  (for/hasheq ([h (in-list hs)])
    (values (string->symbol (~a (header-field h)))
            (~a (header-value h)))))
             
(define handler? (-> request? response?))
(define wrapper? (-> handler? handler?))

(define/contract ((wrap-timed-and-logged handler) req) wrapper?
  (define t0 (current-inexact-milliseconds))

  (define resp (handler req))

  (define t1 (current-inexact-milliseconds))
  (define dur (round (- t1 t0)))

  ;; Let's use "structured logging" here to make it easier to search,
  ;; and do things like create CloudWatch metrics from CloudWatch Logs
  ;; filters (they have a syntax to extract things from JSON.)
  (log-info
   (jsexpr->string
    (hasheq 'request  (hasheq 'method  (~a (request-method req))
                              'ip      (request-client-ip req)
                              'path    (url->string (request-uri req))
                              'headers (headers->hasheq (request-headers/raw req)))
            'response (hasheq 'code     (response-code resp)
                              'headers  (headers->hasheq (response-headers resp))
                              'duration dur))))

  resp)

(define (error-responder url exn)
  (log-error "Exception responding to ~v:\n~a"
             (url->string url)
             (format "~s" exn))
  (response/full 500 #"Oops"
                 (current-seconds)
                 #f '() '()))

#|
    curl --silent http://localhost:8000/servlets/standalone.rkt --write-out '%{stderr}http_code=%{http_code}\n' | time wc -l

Current behavior:
    Exception responding to "/servlets/standalone.rkt":
      #(struct:exn:fail:contract:blame "serve/servlet: 
      contract violation
          expected: can-be-response?
          given: #<procedure:.../tcpdrop/serve3.rkt:94:0>
          in: the range of
              the 1st argument of
              (->*
              ((-> request? can-be-response?))
              (#:banner?
                boolean?
                #:command-line?
                boolean?
                #:connection-close?
                boolean?
                #:extra-files-paths
                (listof path-string?)
                #:file-not-found-responder
                (-> request? can-be-response?)
                #:launch-browser?
                boolean?
                #:listen-ip
                (or/c #f string?)
                #:log-file
                (or/c #f path-string? output-port?)
                #:log-format
                (or/c
                (or/c
                  'parenthesized-default
                  'extended
                  'apache-default)
                (-> request? string?))
                #:manager
                manager?
                #:max-waiting
                (>=/c 0)
                #:mime-types-path
                path-string?
                #:port
                listen-port-number?
                #:quit?
                boolean?
                #:safety-limits
                safety-limits?
                #:server-root-path
                path-string?
                #:servlet-current-directory
                path-string?
                #:servlet-loading-responder
                (-> url? any/c can-be-response?)
                #:servlet-namespace
                (listof module-path?)
                #:servlet-path
                string?
                #:servlet-regexp
                regexp?
                #:servlet-responder
                (-> url? any/c can-be-response?)
                #:servlets-root
                path-string?
                #:ssl-cert
                (or/c #f path-string?)
                #:ssl-key
                (or/c #f path-string?)
                #:ssl?
                boolean?
                #:stateless?
                boolean?
                #:stuffer
                (stuffer/c
                (-> serializable? bytes?)
                (-> bytes? serializable?)))
              any)
          contract from: 
              <pkgs>/web-server-lib/web-server/servlet-env.rkt
          blaming: /home/jeff/srchome/pmi/src/precontrib/tcpdrop/serve3.rkt
          (assuming the contract is correct)
          at: <pkgs>/web-server-lib/web-server/servlet-env.rkt:44.2" #<continuation-mark-set> #<blame-no-swap>)
|#

(define try1 (-> ;Note: requests go UP this chain, responses DOWN
                my-app
                ; wrap-gzip
                ; wrap-not-modified
                ; wrap-authorize
                ; wrap-authenticate
                ; wrap-http->https
                wrap-timed-and-logged))

(define ((try2 handler) req)
    (define x1 (wrap-timed-and-logged req))
    (define x2 (my-app x1))
    (define x3 (x2 handler))
    (define x4 (x1 x3))
    x4)

(serve/servlet try2
               #:launch-browser? #f
               #:servlet-path      "/"
               #:servlet-regexp    #px""
               #:listen-ip         #f
               #:port              8000
               #:servlet-responder error-responder)

; (serve/servlet 
;   my-app
;   #:launch-browser? #f
;   #:log-format 'extended
;   #:log-file (current-output-port))


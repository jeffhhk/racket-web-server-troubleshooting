#lang racket
#|
    serve8.rkt: The response logging mechanism of serve6b.rkt, ala carte.

    Note that use of this mechanism requires bypassing the web-server/dispatchers in order to expose
    the connection object.

    Log responses as well as requests.

    Note that dropped TCP connection events are not exposed by web-server, so we can't log them
    directly.  In principle, all the information logged in this example could be combined with 
    a requestID to produce a corpus which could detect requests that did not produce response 
    events.  Dropped TCP connections would generate matching query results.
|#
(require
        web-server/web-server
        web-server/http
        web-server/http/response
         web-server/servlet
         web-server/servlet-env
         json)
         
(define eol-bytes (string->bytes/utf-8 "\n"))

(define i-max 10000000)

(define (long-response op)
  (for ((i (range 1 (+ 1 i-max))))
                  (write-bytes (jsexpr->bytes `#hasheq((i . ,i))) op)
                  (write-bytes eol-bytes op)))


(define (base-path-of-url url)
    (define path (url-path url))
    ;; "5 = path (two elements),"
    ;; https://docs.racket-lang.org/net/url.html
    (map path/param-path path))


; loosely based on:
;   https://stackoverflow.com/questions/53911162/how-to-show-http-status-code-in-web-server-logs
(define (headers->hasheq hs)
  (for/hasheq ([h (in-list hs)])
    (values (string->symbol (~a (header-field h)))
            (~a (header-value h)))))

(define (logwrap-request-impl req)
    (hasheq 'method  (~a (request-method req))
            'ip      (request-client-ip req)
            'path    (url->string (request-uri req))
            'headers (headers->hasheq (request-headers/raw req))))

(define ((logwrap-lazy handler) req)
  (define t0 (current-inexact-milliseconds))
  (displayln
    (jsexpr->string
      (hasheq 'request  (logwrap-request-impl req))))

  (define resp (handler req))
  
  (struct-copy response resp (output
               (lambda (fd)
                 (define tmp ((response-output resp) fd))
                 (define t1 (current-inexact-milliseconds))
                 (define dur (round (- t1 t0)))
                 
                 ;; Let's use "structured logging" here to make it easier to search,
                 ;; and do things like create CloudWatch metrics from CloudWatch Logs
                 ;; filters (they have a syntax to extract things from JSON.)
                 (displayln
                  (jsexpr->string
                   (hasheq 'request  (logwrap-request-impl req)
                           'response (hasheq 'code     (response-code resp)
                                             'headers  (headers->hasheq (response-headers resp))
                                             'duration dur))))
                 tmp))))

(define (my-app-dispatch req)
    (define t0 (current-milliseconds))
    (define uripath (base-path-of-url (request-uri req)))
    (define m (request-method req))
    (define fn
            (cond
                ((equal? uripath `("long-response")) long-response)
                (else #f)))
    (if fn
        (response
            200 #"OK"
            t0 TEXT/HTML-MIME-TYPE
            empty
            fn)
        (response
            404 #"Not found"
            t0 TEXT/HTML-MIME-TYPE
            empty
            (lambda (op) #f))))
            


#|
    test with:
        curl --silent http://localhost:8000/long-response --write-out '%{stderr}http_code=%{http_code}\n' | time wc -l

|#


(serve/servlet 
  (logwrap-lazy my-app-dispatch)
  #:launch-browser? #f
; not compatible with this approach:  #:servlet-path "/"
  #:servlet-regexp #rx""
  #:log-format (lambda (req) "") ; disable built-in logging so we can log responses and requests in the same way
  #:log-file (current-output-port))


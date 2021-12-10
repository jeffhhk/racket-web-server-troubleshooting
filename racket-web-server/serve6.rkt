#lang racket
#|
    serve6.rkt: builds upon serve1c.rkt workaround, allowing the dispatch path to be controlled.

    Demonstrate serving a slow (as opposed to long) response in racket.
    
    Show how server responds to a dropped TCP connection.
    
    Includes writing whitespace to the output channel, as a workaround to causing the racket computation
    to actually get killed.

    Note that use of this mechanism requires bypassing the web-server/dispatchers in order to expose
    the connection object.
|#
(require
        web-server/web-server
        web-server/http
        web-server/http/response
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

(define i-max 10000000)
(define (slow-response op)
  (for ((i (range 1 (+ 1 i-max))))
    (if (equal? i i-max)
        (begin
            (write-bytes (jsexpr->bytes `#hasheq((i . ,i))) op)
            (write-bytes eol-bytes op))
        (jsexpr->bytes `#hasheq((i . ,i))))
    (when (equal? (modulo (+ i 1) 10000) 0)
        ; does not allow death detection:
        (sleep 0.001)
        ; does not allow death detection:
        (display " " (current-output-port))
        (flush-output)
        ; *does allow* death detection, but does not allow "finished in" to be triggered:
        (display " " op)
        (flush-output op)
        )))

(define (long-response op)
  (for ((i (range 1 (+ 1 i-max))))
                  (write-bytes (jsexpr->bytes `#hasheq((i . ,i))) op)
                  (write-bytes eol-bytes op)))


(define (base-path-of-url url)
    (define path (url-path url))
    ;; "5 = path (two elements),"
    ;; https://docs.racket-lang.org/net/url.html
    (map path/param-path path))

#| Tests:
|#

(define (my-app-dispatch req)
    (printf "my-app-1 uri=~s\n" (request-uri req))
    (define t0 (current-milliseconds))
    (define uripath (base-path-of-url (request-uri req)))
    (printf "my-app-1 uri-path=~s\n" uripath)
    (define fn
            (cond
                ((equal? uripath `("slow-response")) (printf "found slow\n") slow-response)
                ((equal? uripath `("long-response")) (printf "found long\n") long-response)
                (else (begin
                    (printf "found else\n")
                    #f))))
    (if fn
        (response
            200 #"OK"
            t0 TEXT/HTML-MIME-TYPE
            empty
            (log-fn fn))
        (response
            404 #"Not found"
            t0 TEXT/HTML-MIME-TYPE
            empty
            (log-fn (lambda (op) #f)))))
            


#|
    test dropped TCP connection behavior with:

        timeout --foreground 3s curl --silent http://localhost:8000/long-response --write-out '%{stderr}http_code=%{http_code}\n' | time wc -l
        timeout --foreground 3s curl --silent http://localhost:8000/slow-response --write-out '%{stderr}http_code=%{http_code}\n' | time wc -l

    test dispatch with:

        curl --silent http://localhost:8000/long-response
        curl --silent http://localhost:8000/long-response?foo=bar
        curl --silent http://localhost:8000/long-response/longer?foo=bar
|#


(serve/servlet 
  my-app-dispatch
  #:launch-browser? #f
; not compatible with this approach:  #:servlet-path "/"
  #:servlet-regexp #rx""
  #:log-format 'extended
  #:log-file (current-output-port))


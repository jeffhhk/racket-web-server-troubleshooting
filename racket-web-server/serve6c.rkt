#lang racket
#|
    serve6c.rkt: builds upon serve6b.rkt/serve6.rkt/serve1c.rkt.

    Demonstrate serving a slow (as opposed to long) response in racket.
    The existing mediKanren 2 web server is often a slow (as opposed to long) response.
    
    Show how server responds to a dropped TCP connection.
    
    Includes writing whitespace to the output channel, as a workaround to causing the racket computation
    to actually get killed.

    Note that use of this mechanism requires bypassing the web-server/dispatchers in order to expose
    the connection object.

    Log responses as well as requests.

    Note that dropped TCP connection events are not exposed by web-server, so we can't log them
    directly.  In principle, all the information logged in this example could be combined with 
    a requestID to produce a corpus which could detect requests that did not produce response 
    events.  Dropped TCP connections would generate matching query results.

    Adds use of the loosely coupled routine keepalive-and-timeout-via-thread.
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

#|
    keepalive-and-timeout-via-thread: uses a background thread to take a background "keepalive"
            action while a foreground action is taking place.

        dsec-between    ; number of seconds between background actions
        dsec-max        ; maximum duration allowed for foreground action
        thunk-keepalive ; background action to be repeated as long as the foreground action runs
        val-for-timeout ; value to return if foreground action takes too long
        thunk           ; foreground action
|#
(define (keepalive-and-timeout-via-thread dsec-between dsec-max thunk-keepalive val-for-timeout thunk)
  (define v 'placeholder)
  (define th-run
    (thread
     (lambda () (set! v (thunk)))))
  (define (iter t-max)
    (if (>= (current-milliseconds) t-max)
        val-for-timeout
        (begin
          (sleep dsec-between)
          (thunk-keepalive)
          (let ((evt (sync/timeout dsec-between th-run)))
            (if (thread? evt)
                v
                (iter t-max))))))
  (iter (+ (current-milliseconds) (* 1000 dsec-max))))

(define i-max 10000000)
(define (slow-response op)
  (keepalive-and-timeout-via-thread 
    1       ; print whitespace every 1 second
    1000000 ; allow running for 1M seconds
    (lambda () ; background action
          (display " " op)                    ; triggers death detection
          (flush-output op)
          (display " " (current-output-port)) ; shows the user that it is running
          (flush-output (current-output-port)))
    #f      ; value to return in the case of timeout
    (lambda () ; foreground action
        (for ((i (range 1 (+ 1 i-max))))
            (if (equal? i i-max)
                (begin
                    (write-bytes (jsexpr->bytes `#hasheq((i . ,i))) op)
                    (write-bytes eol-bytes op))
                (jsexpr->bytes `#hasheq((i . ,i))))))))

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
    (printf "my-app-1 uri=~s\n" (request-uri req))
    (define t0 (current-milliseconds))
    (define uripath (base-path-of-url (request-uri req)))
    (printf "my-app-1 uri-path=~s\n" uripath)
    (define m (request-method req))
    (printf "my-app-i m=~s\n" m)
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
  (logwrap-lazy my-app-dispatch)
  #:launch-browser? #f
; not compatible with this approach:  #:servlet-path "/"
  #:servlet-regexp #rx""
  #:log-format (lambda (req) "") ; disable built-in logging so we can log responses and requests in the same way
  #:log-file (current-output-port))


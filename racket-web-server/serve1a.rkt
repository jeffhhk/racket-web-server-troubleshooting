#lang racket
#|
  Demonstrate serving a long (streaming) response in racket.
  Show how server responds to a dropped TCP connection.
  A dropped TCP connection causes dynamic-wind fail to call the cleanup thunk.  (A thread-based phenomenon?)
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

(define (my-app1 req)
  (define t0 (current-milliseconds))
  (response
    200 #"OK"
    t0 TEXT/HTML-MIME-TYPE
    empty
    (log-fn long-response)))
 
#|
    curl --silent http://localhost:8000/servlets/standalone.rkt --write-out '%{stderr}http_code=%{http_code}\n' | time wc -l
causes server logs of:
    ((client-ip "127.0.0.1") (host-ip "127.0.0.1") (referer #f) (uri "/servlets/standalone.rkt") (time 1632949740))
    finished in 10615ms

but
    timeout --foreground 5s curl --silent http://localhost:8000/servlets/standalone.rkt --write-out '%{stderr}http_code=%{http_code}\n' | time wc -l
causes server logs of:
    ((client-ip "127.0.0.1") (host-ip "127.0.0.1") (referer #f) (uri "/servlets/standalone.rkt") (time 1632949632))

No "finished in" is emitted.

|#

(serve/servlet 
  my-app1
  #:launch-browser? #f
  #:log-format 'extended
  #:log-file (current-output-port))


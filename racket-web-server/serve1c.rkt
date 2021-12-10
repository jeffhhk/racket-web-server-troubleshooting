#lang racket
#|
  Demonstrate serving a slow (as opposed to long) response in racket.
  
  Show how server responds to a dropped TCP connection.
  
  Includes writing whitespace to the output channel, as a workaround to causing the racket computation
  to actually get killed.
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

(define (my-app1 req)
  (define t0 (current-milliseconds))
  (response
    200 #"OK"
    t0 TEXT/HTML-MIME-TYPE
    empty
    (log-fn slow-response)))
 
(define (my-app2 req)
  (define t0 (current-milliseconds))
  ((log-fn response)
    200 #"OK"
    t0 TEXT/HTML-MIME-TYPE
    empty
    slow-response))

#|
    trigger with:
    
    timeout --foreground 3s curl --silent http://localhost:8000/servlets/standalone.rkt --write-out '%{stderr}http_code=%{http_code}\n' | time wc -l
|#


(serve/servlet 
  my-app1
  #:launch-browser? #f
  #:log-format 'extended
  #:log-file (current-output-port))


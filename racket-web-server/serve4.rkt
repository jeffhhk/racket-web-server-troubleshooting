#lang racket
#|
  An experiment to see if the serve/servelet #:servlet-responder parameter can improve error logging.
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

(define (my-app2 req)
  (define t0 (current-milliseconds))
  ((log-fn response)
    200 #"OK"
    t0 TEXT/HTML-MIME-TYPE
    empty
    long-response))

(define (error-responder url exn)
  (log-error "Exception responding to ~v:\n~a"
             (url->string url)
             (format "~s" exn))
  (response/full 500 #"Oops"
                 (current-seconds)
                 #f '() '()))

;; timeout --foreground 3s curl --silent http://localhost:8000/servlets/standalone.rkt --write-out '%{stderr}http_code=%{http_code}\n' | time wc -l

(serve/servlet 
  my-app2
  #:launch-browser? #f
  #:servlet-responder error-responder
  #:log-format 'extended
  #:log-file (current-output-port))


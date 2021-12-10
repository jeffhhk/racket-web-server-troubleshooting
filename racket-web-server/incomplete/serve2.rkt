#lang racket
#|
  An incomplete WIP attempting to combine "serve" (as opposed to serve/servlet), web-server/dispatchers,
  along with ability to expose the HTTP connection port.
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
 
(serve #:dispatch
       (sequencer:make
        (filter:make
         #rx"/long-response"
          (lambda (conn req)
           (printf "starting /long-response ~a ~a\n" conn req)
           (output-response/method
            conn
            (long-response (connection-o-port conn))
            (request-method req))))
            #|  curl: (1) Received HTTP/0.9 when not allowed
                http_code=000
            |#
        (filter:make
         #rx"/limited"
         (limit:make
          5
          (lambda (conn req)
           (printf "starting /limited ~a ~a\n" conn req)
           (output-response/method
            conn
            (response/full
             200 #"Okay"
             (current-seconds) TEXT/HTML-MIME-TYPE
             empty
             (list (string->bytes/utf-8
                    (format "hello world ~a"
                           (sort (build-list 100000 (λ x (random 1000)))
                                 <)))))
            (request-method req)))
         #:over-limit 'block))
       (lambda (conn req)
         (output-response/method
          conn
          (response/full 200 #"Okay"
                         (current-seconds) TEXT/HTML-MIME-TYPE
                         empty
                         (list #"<html><body>Nothing to see here</body></html>"))
          (request-method req))))
      #:port 8000)

#|
    curl http://localhost:8000/long-response --write-out '%{stderr}http_code=%{http_code}\n'

  Current behavior (unintended):

      curl: (1) Received HTTP/0.9 when not allowed

      http_code=000
|# 

(do-not-return)
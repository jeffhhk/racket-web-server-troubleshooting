#lang racket

;; Tony Garnock-Jones, https://groups.google.com/g/racket-users/c/H43jr8QuM-4
;;   ". . . if the connection breaks, the ports will be closed as usual. . . ."
;;   ". . . Ah, sorry, try `eof-evt` instead of `port-closed-evt`."

(define (handle-by-event in out k)
  ; Discard the HTTP request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  (displayln "request received")
  (flush-output (current-output-port))
  ;; Detect half-closed TCP connections with eof-evt on the input port
  (let ((evt (sync/timeout 10 (eof-evt in))))
    (if evt
      (begin
        (displayln "Error: Detected half-closed TCP connection.")
        (k))
      (begin
        (displayln "about to send response")
        (flush-output (current-output-port))
        (display "HTTP/1.0 200 Okay\r\n" out)
        (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
        (display "<html><body>Hello, world!</body></html>" out)
        (k)))))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle-by-event in out (lambda ()
    (displayln "request processing completed")
  ))
  (close-input-port in)
  (close-output-port out))

(define (serve port-no)
  (define max-num-connections-to-wait 5)
  (define listener (tcp-listen port-no max-num-connections-to-wait #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))

(serve 8000)

#|
    Test with curl and GNU coreutils timeout:
        timeout --foreground 3s curl http://127.0.0.1:8000
    output:
        request received
        got evt
        request processing completed
    
    Tested on:
        Racket v8.0 cs
        on Ubuntu 20.04.4 LTS
|#

#lang racket/base
#|
    https://github.com/racket/web-server/blob/master/web-server-lib/web-server/http/response.rkt:

        (define (output-response-body/chunked conn bresp)
          ;; Flush the headers immediately since the response handler can wait
          ;; a while before writing anything out to the output port.
          (flush-output (connection-o-port conn))

          (define-values (from-servlet to-chunker) (make-pipe))
          (define to-client (connection-o-port conn))
          (define to-chunker-t
            (thread
            (λ ()
              ;; When errors occur, we immediately kill the connection to
              ;; make it clear to the client that something went wrong.  By
              ;; doing this, we ensure that the terminating chunk won't be
              ;; sent and the client can't misinterpret the response as
              ;; successful.  This is in line with how other web servers
              ;; behave.
              ;;
              ;; See: https://github.com/racket/web-server/pull/93
              (with-handlers ([exn:fail?
                                (lambda (e)
                                  (kill-connection! conn)
                                  (close-output-port to-chunker)
                                  (raise e))])
                ((response-output bresp) to-chunker)
                (close-output-port to-chunker)))))

    Can the enable-break API somehow resurrect the ability to catch the dropped connection
    event?

    It appears not.
|#

(require web-server/http
         web-server/servlet-dispatch
         web-server/web-server)

(define stop
  (serve
   #:port 8000
   #:dispatch (dispatch/servlet
               (lambda (req)
                 (response/output
                  (lambda (out)
                    (displayln "hello" out)
                    (sleep 5)
                    (error 'failed)))))))

(with-handlers ([exn?
                 (lambda (e)
                   (printf "caught exn:break e=~s\n" e)
                   (stop))])
  (sync/enable-break never-evt))

#|
Test with (per HTTP protocol, be sure to follow "GET ..." with a double EOL):

telnet localhost 8000

GET / HTTP/1.1

|#

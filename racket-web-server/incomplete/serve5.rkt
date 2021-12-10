#lang racket
#|
    An attempt to create an alternate path for starting "serve" (web-server-lib/web-server/web-server.rkt)
    with different error handling code for dropped TCP connections, using code copy-pasted from web-server.
|#

(require
        web-server/web-server
        web-server/http
        web-server/http/response
        json
        ;; web-server-lib/web-server/web-server.rkt
        web-server/private/connection-manager
         (prefix-in limit: web-server/dispatchers/limit)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)        
         web-server/servlet
         web-server/servlet-env
         net/tcp-sig
         (prefix-in raw: net/tcp-unit)
         web-server/safety-limits
         web-server/private/dispatch-server-sig
         web-server/private/dispatch-server-unit
         web-server/dispatchers/dispatch
         (prefix-in http: web-server/http/request)
         (prefix-in http: web-server/http/response)
         ;; web-server-lib/web-server/private/dispatch-server-with-connect-unit.rkt
         racket/match
         web-server/private/web-server-structs
         web-server/safety-limits
         net/tcp-sig
         ;(prefix-in config: (only-in tcp^ dispatch-server-config*^ ))
         mzlib/thread
         racket/async-channel
         ;web-server/dispatchers/dispatch-server-config
         )



;; copy-paste from web-server-lib/web-server/private/dispatch-server-with-connect-unit.rkt
(define-signature toy-factory^
  (build-toys  ; (integer? -> (listof toy?))
   repaint     ; (toy? symbol? -> toy?)
   toy?        ; (any/c -> boolean?)
   toy-color)) ; (toy? -> symbol?)

(define-signature my-serve^
  ())

;(define-unit foo@
;   (import tcp^)
;   (export toy-factory^)
;     (match-define (safety-limits
;                 #:max-waiting max-waiting
;                 #:response-timeout response-timeout)
;     safety-limits)

;(define-unit foo@
;   (import tcp^
;             (prefix dispatch-server-connect: dispatch-server-connect^)
;             (prefix config: dispatch-server-config*^))
;   (export toy-factory^)
;     (match-define (safety-limits
;                 #:max-waiting config:max-waiting
;                 #:response-timeout config:response-timeout)
;         config:safety-limits)

    ; (define config:safety-limits (make-safety-limits))
    ; (define config:port 8000)
    ; (define config:max-waiting 10)
    ; (define config:listen-ip "0.0.0.0")

; (define-unit foo@
;   (import tcp^
;             (prefix dispatch-server-connect: dispatch-server-connect^)
;             (prefix config: dispatch-server-config*^))
;   (export my-serve^)

    (define config:safety-limits (make-safety-limits))
    (define config:port 8000)
    (define config:max-waiting 10)
    (define config:listen-ip "0.0.0.0")


  (define-unit ssl:dispatch-server-connect@
    (import) (export dispatch-server-connect^)
    (define (port->real-ports ip op)
      (ports->ssl-ports	ip op
                        #:mode 'accept
                        #:context the-ctxt)))

    (define (async-channel-put* ac v)
        (when ac
            (async-channel-put ac v)))


    ;; serve: -> -> void
    ;; start the server and return a thunk to shut it down
    (define (serve1 #:confirmation-channel [confirmation-channel #f])
    (define the-server-custodian (make-custodian))
    (parameterize ([current-custodian the-server-custodian]
                    [current-server-custodian the-server-custodian]
                    #;[current-thread-initial-stack-size 3])
        (define cm (start-connection-manager #:safety-limits config:safety-limits))
        (thread
        (lambda ()
        (run-server
            ;; This is the port argument, but because we specialize listen, it is ignored.
            1
            (handle-connection/cm cm)
            #f
            (lambda (exn)
            ((error-display-handler)
            (format "Connection error: ~a" (exn-message exn))
            exn))
            (lambda (_ mw re)
            (with-handlers ([exn?
                            (λ (x)
                                (async-channel-put* confirmation-channel x)
                                (raise x))])
                (define listener
                (tcp-listen config:port config:max-waiting #t config:listen-ip))
                (let-values
                    ([(local-addr local-port end-addr end-port)
                    (tcp-addresses listener #t)])
                (async-channel-put* confirmation-channel local-port))
                listener))
            tcp-close
            tcp-accept
            tcp-accept/enable-break))))
    (lambda ()
        (custodian-shutdown-all the-server-custodian)))

    ;; handle-connection : connection-manager input-port output-port (input-port -> string string) -> void
    (define ((handle-connection/cm cm)
            i-ip i-op
            #:port-addresses [real-port-addresses tcp-addresses])
    (define-values (ip op)
        (dispatch-server-connect:port->real-ports i-ip i-op))

    (define (port-addresses some-ip)
        (if (eq? ip some-ip)
            (real-port-addresses i-ip)
            (real-port-addresses ip)))

    (define conn
        (new-connection cm ip op (current-custodian) #f))

    (with-handlers ([exn-expected?
                    (lambda (_)
                        (kill-connection! conn))])
        (let connection-loop ()
        (define-values (req close?)
            (config:read-request conn config:port port-addresses))
        (reset-connection-timeout! conn config:response-timeout)
        (set-connection-close?! conn close?)
        (config:dispatch conn req)
        (if (connection-close? conn)
            (kill-connection! conn)
            (connection-loop)))))


    ;; copy-paste from web-server-lib/web-server/web-server.rkt
    (define (serve
            #:dispatch dispatch
            #:confirmation-channel [confirmation-channel #f]
            #:connection-close? [connection-close? #f]
            #:dispatch-server-connect@ [dispatch-server-connect@ raw:dispatch-server-connect@]
            #:tcp@ [tcp@ raw:tcp@]
            #:port [port 80]
            #:listen-ip [listen-ip #f]
            #:max-waiting [_max-waiting 511]
            #:initial-connection-timeout [_timeout 60]
            #:safety-limits [safety-limits (make-safety-limits
                                            #:max-waiting _max-waiting
                                            #:request-read-timeout _timeout)])
    (define read-request
        (http:make-read-request
        #:connection-close? connection-close?
        #:safety-limits safety-limits))
    (define-unit-binding a-dispatch-server-connect@
        dispatch-server-connect@ (import) (export dispatch-server-connect^))
    (define-unit-binding a-tcp@
        tcp@ (import) (export tcp^))
    (define-compound-unit/infer dispatch-server@/tcp@
        (import dispatch-server-config*^)
        (link a-dispatch-server-connect@ a-tcp@ dispatch-server-with-connect@)
        (export dispatch-server^))
    (define-values/invoke-unit
        dispatch-server@/tcp@
        (import dispatch-server-config*^)
        (export dispatch-server^))
    (serve1 #:confirmation-channel confirmation-channel))


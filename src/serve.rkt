#lang racket

; This file will run the servlet, restarting whenever a file system change is
; detected in the current directory.

(require racket/rerequire)
(require web-server/servlet-env)

(dynamic-rerequire (string->path "src/servlet.rkt"))
(define start (dynamic-require "src/servlet.rkt" 'start))

(define (run-server)
  (serve/servlet start
    #:launch-browser? #f
    #:port 8000
    #:server-root-path (current-directory)
    #:servlet-regexp #rx""))
    ;#:listen-ip "::1"))
    ;#:listen-ip "2001:4978:f:683::2"))

(define (main)
  (let loop ()
    ; create a filesystem change event
    (define change-watcher (filesystem-change-evt (current-directory)))
    ; run the server
    (define cust (make-custodian))
    (parameterize ([current-custodian cust])
      (thread run-server))
    ; wait until a filesystem change event is received
    (sync change-watcher)
    ; shutdown server
    (custodian-shutdown-all cust)
    ; reload servlet.rkt if necessary
    (with-handlers
      ([exn:fail? (lambda (e) (displayln (~a "error ~a" e)))])
      (dynamic-rerequire "src/servlet.rkt")
      (set! start (dynamic-require "src/servlet.rkt" 'start)))
    (displayln "Restarting server...")
    (loop)))

(main)

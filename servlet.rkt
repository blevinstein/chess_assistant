#lang racket

(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/templates)

(define (main)
  (serve/servlet start
    #:servlet-regexp #rx""
    #:port 8000
    #:launch-browser? #f))

(define (start req)
  (log-info (url->string (request-uri req)))
  (handle-request req))

(define-values (handle-request path-to)
  (dispatch-rules
    [("") home]
    ))

(define (home req)
  (response/output
    (lambda (out) (display (include-template "index.html") out))))

(main)

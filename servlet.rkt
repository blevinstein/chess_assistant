#lang racket

(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/templates)

(define (main)
  (serve/servlet start
    #:launch-browser? #f
    #:port 8000
    #:server-root-path (current-directory)
    #:servlet-regexp #rx""))

; entry point for serving a request
(define (start req)
  (log-info (url->string (request-uri req)))
  (handle-request req))

; render a page from a template
(define (render template)
  (response/output
    (lambda (out) (display template out))))

; routing definitions
(define-values (handle-request path-to)
  (dispatch-rules
    [("") home]
    ))

(define (home req)
  (render (include-template "template/index.html")))

(main)

#lang racket

(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/templates)

;(no-web-browser)
;(static-files-path "web")

(require web-server/dispatch)
(define-values (handle-request path-to)
  (dispatch-rules
    [else home]
    ))

(define (home req)
  (log-info (url->string (request-uri req)))
  (response/output
    (lambda (out) (display (include-template "hello.html") out))))

(serve/servlet handle-request
  #:servlet-regexp #rx""
  #:port 8000
  #:launch-browser? #f
  #:extra-files-paths (list "web"))

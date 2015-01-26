#lang racket

(require json)
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/templates)

(require "chess.rkt")

; TODO rpc get-hud

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

(define (render-json expr)
  (response/output
    (lambda (out) (write-json expr out))))

; routing definitions
(define-values (handle-request path-to)
  (dispatch-rules
    [("") home]
    [("new-board") new-board]
    ))

(define (home req)
  (redirect-to "index.html" temporarily))

(define (new-board req)
  (render-json (grid->json (new-grid))))

(define (grid->json grid)
  (define (xfm-cp cp)
    (match cp
      [(cons color piece) (hash 'color (~a color)
                                'piece (~a piece))]
      [#f 'null]))
  (define (xfm-row row)
    (map xfm-cp row))
  (map xfm-row grid))

(main)


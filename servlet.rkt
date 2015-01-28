#lang racket

(require json)
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/templates)

(require "chess.rkt")

; TODO rpc make-move
; TODO rpc get-hud

(provide main)
(define (main)
  (serve/servlet start
    #:launch-browser? #f
    #:port 8000
    #:server-root-path (current-directory)
    #:servlet-regexp #rx""))

; entry point for serving a request
(define (start req)
  (log-info "request for ~a" (url->string (request-uri req)))
  (handle-request req))

; render a page from a template
(define (render template)
  (response/output
    (lambda (out) (display template out))))

(define (render-json expr)
  (response/output
    (lambda (out) (write-json expr out))))

(define (render-error message)
  (response/output
    (lambda (out) (write message out))
    #:code 500))

; routing definitions
(define-values (handle-request path-to)
  (dispatch-rules
    [("") home]
    [("new-board") new-board]
    [("moves") #:method "post" moves]
    ))

(define (home req)
  (log-info "home")
  (redirect-to "index.html" temporarily))

(define (new-board req)
  (log-info "new-board")
  (render-json (grid->json (new-grid))))

; JSON conversion code

; *->json

(provide position->json)
(define (position->json position)
  (for/list ([cpl position])
    (match cpl [(list color piece location)
      (hash 'color (~a color)
            'piece (~a piece)
            'loc (~a location)
            `repr (piece-code piece color)
            )])))

(provide grid->json)
(define (grid->json grid)
  (define (xfm-cp cp)
    (match cp
      [(cons color piece) (hash 'color (~a color)
                                'piece (~a piece)
                                'repr (piece-code piece color)
                                )]
      [#f 'null]))
  (define (xfm-row row) (map xfm-cp row))
  (map xfm-row grid))

(provide location->json)
(define (location->json loc)
  (match loc [(location f r) (list f r)]))

(provide move->json)
(define (move->json mv)
  (define (xfm-mv move-part) (match move-part [(move s d) (map location->json (list s d))]))
  (map xfm-mv mv))

; json->*

(provide json->grid)
(define (json->grid json)
  (define (xfm-cp cp)
    (match cp
      [(hash-table ('color c) ('piece p) (k v) ...) (cons (string->symbol c) (string->symbol p))]
      ['null #f]))
  (define (xfm-row row) (map xfm-cp row))
  (map xfm-row json))

; EXPERIMENTAL below this line

(define (moves req)
  (define post-data (request-post-data/raw req))
  (log-info "moves ~a" post-data)
  (define parsed-req (bytes->jsexpr post-data))
  ; params
  (define position (grid->position (json->grid (hash-ref parsed-req 'grid))))
  (define rank (string->number (hash-ref parsed-req 'rank)))
  (define file (string->number (hash-ref parsed-req 'file)))

  (define source (location file rank))
  (with-handlers
    ([exn:fail? (lambda (s) (log-error "moves error ~a" s) (render-error s))])
    (render-json (move->json (possible-moves position source)))))

; To execute main: racket servlet.rkt .
(when (vector-member "." (current-command-line-arguments)) (main))


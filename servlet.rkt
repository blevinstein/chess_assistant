#lang racket

(require json)
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/templates)

(require "chess.rkt")

; entry point for serving a request
(provide start)
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
    [("try-move") #:method "post" try-move]
    ))

; rpc handlers

(define (home req)
  (log-info "home")
  (redirect-to "index.html" temporarily))

(define (new-board req)
  (log-info "new-board")
  (render-json (position->json (new-position))))

(define (moves req)
  (define parsed-req (bytes->jsexpr (request-post-data/raw req)))
  (log-info "moves ~a" parsed-req)
  ; params
  (define position (json->position (hash-ref parsed-req 'position)))
  (define source (json->location (hash-ref parsed-req 'loc)))

  (with-handlers
    ([exn:fail? (lambda (e) (log-error "moves error ~a" e) (render-error e))])
    (render-json (move->json (valid-moves position source)))))

(define (try-move req)
  (define parsed-req (bytes->jsexpr (request-post-data/raw req)))
  ; params
  (define position (json->position (hash-ref parsed-req 'position)))
  (define source (json->location (hash-ref parsed-req 'source)))
  (define dest (json->location (hash-ref parsed-req 'dest)))

  (with-handlers
    ([exn:fail? (lambda (e) (log-error "try-move error ~a" e) (render-error e))])
    (define mv (move source dest))
    (if (valid-move position mv)
      (render-json (position->json (make-move position (move source dest))))
      (raise-argument-error "try-move" "valid move" (move source dest)))))

; JSON conversion code
; TODO refactor into separate file

; *->json

; TODO include empty squares in position
(provide position->json)
(define (position->json position)
  (for/list ([cpl position])
    (match cpl [(list color piece loc)
      (hash 'color (~a color)
            'piece (~a piece)
            'loc (location->json loc)
            `repr (piece-code piece color)
            'threatCount (threat-count position loc)
            'overloadedDefender (overloaded-defender? position loc)
            'attackedByLowerValue #f ; TODO fix
            ;'attackedByLowerValue (attacked-by-lower-value? position loc)
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

(provide json->position)
(define (json->position json)
  (for/list ([elem json])
    (match elem
      [(hash-table ('color c) ('piece p) ('loc l) (k v) ...)
       (list (string->symbol c) (string->symbol p) (json->location l))])))

(provide json->location)
(define (json->location json)
  (match json [(list f r) (location f r)]))

#lang typed/racket

(displayln "Typed chess loaded")

(struct: location ([file : Integer] [rank : Integer]))

(define-type ColorPiece (Pair Symbol Symbol))

(define-type PieceLocation (Pair Symbol location))

(struct: color-piece-location ([color : Symbol] [piece : Symbol] [location : location]))

(define-type Grid (Listof (Listof ColorPiece)))

(define-type Position (Listof color-piece-location))

(define-type Move (Pair location location))

; gives the point value of each piece
(provide piece-values)
(: piece-values (-> Symbol Integer))
(define (piece-values piece)
  (hash-ref #hash(
    (P . 1)
    (N . 3)
    (B . 3)
    (R . 5)
    (Q . 9)
    (K . 0)) piece))

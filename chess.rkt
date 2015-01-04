#lang racket

; piece data

(define piece-values
  (hash
    'P 1
    'N 3
    'B 3
    'R 5
    'Q 9
    'K 0))

(define piece-codes
  (hash
    'K "\u265a"
    'Q "\u265b"
    'R "\u265c"
    'B "\u265d"
    'N "\u265e"
    'P "\u265f"))

(define file-string
  (hash
    0 "a"
    1 "b"
    2 "c"
    3 "d"
    4 "e"
    5 "f"
    6 "g"
    7 "h"))

(define back-row
  '(R N B Q K B N R))

; terminal escape sequences

(define (esc . codes)
  (string-append
    "\e["
    (string-join (map number->string codes) ";")
    "m"))

(define bg-white (esc 48 5 246))
(define bg-black (esc 48 5 240))
(define fg-white (esc 97))
(define fg-black (esc 30))
(define reset (esc 0))

; definition of a new board

(define (new-grid)
  (append
    (list (map (λ (piece) (cons 'black piece)) back-row))
    (list (build-list 8 (λ (i) (cons 'black 'P))))
    (build-list 4 (λ (i) (build-list 8 (λ (i) null))))
    (list (build-list 8 (λ (i) (cons 'white 'P))))
    (list (map (λ (piece) (cons 'white piece)) back-row))))

; print subroutines

(define (print-square background square)
  (display (if (equal? background 'black) bg-black bg-white))
  (display " ")
  (match square
    [(cons player piece)
      (display (if (equal? player 'black) fg-black fg-white))
      (display (hash-ref piece-codes piece))]
    [null (display " ")])
  (display " "))

(define (print-grid grid)
  (display "  ")
  (for ([j (in-range 8)])
    (display " ")
    (display j)
    (display " "))
  (displayln "")
  (for ([i (in-range 8)])
    (display (hash-ref file-string i))
    (display " ")
    (for ([j (in-range 8)])
      (define square (list-ref (list-ref grid i) j))
      (print-square (if (equal? (modulo (+ i j) 2) 0) 'black 'white) square))
    (displayln reset)))

; grid->position conversion code

(define (grid-player-position grid player)
  (filter (compose1 not null?)
    (for*/list ([rank (in-range 8)] [file (in-range 8)])
      (define square (list-ref (list-ref grid rank) file))
      (match square
        [(cons color piece)
         (if (equal? color player)
             (cons piece (cons file rank))
             null)]
        [_ null]))))

(define (grid->position grid)
  (cons
    (grid-player-position grid 'white)
    (grid-player-position grid 'black)))

(define (new-position) (grid->position (new-grid)))

(define (position-white position) (car position))
(define (position-black position) (cdr position))

(print-grid (new-grid))

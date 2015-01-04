#lang racket

(define piece-values
  (hash
    'P 1
    'N 3
    'B 3
    'R 5
    'Q 9
    'K 0))

(define back-row
  '(R N B Q K B N R))

(define (esc . codes)
  (string-append
    "\e["
    (string-join (map number->string codes) ";")
    "m"))

(define bg-white (esc 47)) ; light gray
(define bg-black (esc 100)) ; dark gray
(define fg-white (esc 97))
(define fg-black (esc 30))
(define reset (esc 0))

(define (new-grid)
  (append
    (list (map (λ (piece) (cons 'black piece)) back-row))
    (list (build-list 8 (λ (i) (cons 'black 'P))))
    (build-list 4 (λ (i) (build-list 8 (λ (i) null))))
    (list (build-list 8 (λ (i) (cons 'white 'P))))
    (list (map (λ (piece) (cons 'white piece)) back-row))))

(define (print-grid grid)
  (for ([row grid] [i (in-range 8)])
    (for ([square row] [j (in-range 8)])
      ; set background color
      (display (if (equal? (modulo (+ i j) 2) 0) bg-black bg-white))
      (match square
        [(cons player piece)
          (display (if (equal? player 'black) fg-black fg-white))
          (display (symbol->string piece))]
        [null (display " ")]))
    (displayln reset)))

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

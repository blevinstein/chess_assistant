#lang racket

; informal types
; location : (rank . file)
; player-piece : (player . piece)
; piece-location : (piece . location)
; grid : ( ( player-piece+ )+ )
;        ( row+ )
; position: ( ( piece-location+ ) . ( piece-location+ ) )
;           ( white-piece-locations . black-piece-locations )

; gives the point value of each piece
(define piece-values
  (hash
    'P 1
    'N 3
    'B 3
    'R 5
    'Q 9
    'K 0))

; gives the unicode character of each piece
(define (piece-code piece)
  (hash-ref (hash
    'K "\u265a"
    'Q "\u265b"
    'R "\u265c"
    'B "\u265d"
    'N "\u265e"
    'P "\u265f")
    piece))

; gives the number representing a rank
(define (rank-string rank)
  (number->string (+ rank 1)))

; gives the letter representing a file
(define (file-string file)
  (hash-ref (hash
    0 "a"
    1 "b"
    2 "c"
    3 "d"
    4 "e"
    5 "f"
    6 "g"
    7 "h")
    file))

; defines the pieces on the back row
(define back-row
  '(R N B Q K B N R))

; used to generate terminal escape sequences
(define (esc . codes)
  (string-append
    "\e["
    (string-join (map number->string codes) ";")
    "m"))

; terminal escape sequences
(define bg-white (esc 48 5 246))
(define bg-black (esc 48 5 240))
(define fg-white (esc 97))
(define fg-black (esc 30))
(define reset (esc 0))

; create a new grid
(define (new-grid)
  (append
    (list (map (λ (piece) (cons 'white piece)) back-row))
    (list (build-list 8 (λ (i) (cons 'white 'P))))
    (build-list 4 (λ (i) (build-list 8 (λ (i) null))))
    (list (build-list 8 (λ (i) (cons 'black 'P))))
    (list (map (λ (piece) (cons 'black piece)) back-row))))

; get a player-piece from a grid
(define (grid-ref grid rank file)
  (list-ref (list-ref grid rank) file))

; print subroutines

(define (print-square background player-piece)
  (display (if (equal? background 'black) bg-black bg-white))
  (display " ")
  (match player-piece
    [(cons player piece)
      (display (if (equal? player 'black) fg-black fg-white))
      (display (piece-code piece))]
    [null (display " ")])
  (display " "))

(define (print-grid grid)
  (display "  ")
  (for ([j (in-range 8)])
    (display " ")
    (display (file-string j))
    (display " "))
  (displayln "")
  (for ([i (in-range 8)])
    (display (rank-string i))
    (display " ")
    (for ([j (in-range 8)])
      (define player-piece (grid-ref grid i j))
      (print-square (if (equal? (modulo (+ i j) 2) 0) 'black 'white) player-piece))
    (displayln reset)))

(define (player-piece-repr player-piece)
  (match player-piece
    [(cons player piece) (string-append (symbol->string player) " " (symbol->string piece))]
    [_ "\u2205"]))

(define (piece-location-repr piece-location)
  (match piece-location
    [(cons piece location) (string-append (piece-code piece) " @ " (location-repr location))]))

(define (location-repr location)
  (match location
    [(cons rank file) (string-append (rank-string rank) (file-string file))]))

; grid->position conversion code

(define (grid->position grid)
  (cons
    (grid-player-position grid 'white)
    (grid-player-position grid 'black)))

(define (grid-player-position grid player)
  (filter (compose1 not null?)
    (for*/list ([rank (in-range 8)] [file (in-range 8)])
      (define player-piece (grid-ref grid rank file))
      (match player-piece 
        [(cons color piece)
         (if (equal? color player)
             (cons piece (cons file rank))
             null)]
        [_ null]))))

; create a new board
(define (new-position) (grid->position (new-grid)))

(define (position-player position player)
  (if (equal? player 'white) (car position) (cdr position)))

; TODO position->grid

; adds two locations together like vectors
(define (add-location a b)
  (cons
    (+ (car a) (car b))
    (+ (cdr a) (cdr b))))

; checks that a location is in [0,8) x [0,8)
(define (in-bounds location)
  (match location
    [(cons rank file) (and
      (positive? rank) (< rank 8)
      (positive? file) (< file 8))]))

; (all-offsets (1 . 2)) = '( (1 . 2) (-1 . 2) (-1 . -2) (1 . -2)
;                            (2 . 1) (2 . -1) (-2 . -1) (-2 . 1) )
; (all-offsets (1 . 1)) = '( (1 . 1) (-1 . 1) (-1 . -1) (1 . -1) )
(define (all-offsets offset)
  (match offset
    [(cons x x) (all-directions offset)]
    [(cons x y) (append (all-directions offset) (all-directions (cons y x)))]))

(define (all-directions offset)
  (match offset
    [(cons rank file) (list (cons rank file) (cons (- rank) file)
                            (cons rank (- file)) (cons (- rank) (- file)))]))

(print-grid (new-grid))


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
(define (grid-ref grid location)
  (match location [(cons rank file)
    (list-ref (list-ref grid rank) file)]))

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
    (display (string-append " " (file-string j) " ")))
  (displayln "")
  (for ([i (in-range 8)])
    (display (string-append (rank-string i) " "))
    (for ([j (in-range 8)])
      (define player-piece (grid-ref grid (cons i j)))
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
      (define player-piece (grid-ref grid (cons rank file)))
      (match player-piece 
        [(cons color piece)
         (if (equal? color player)
             (cons piece (cons file rank))
             null)]
        [_ null]))))

; create a new position
(define (new-position) (grid->position (new-grid)))

(define position-white car)
(define position-black cdr)

(define (position-player position player)
  (if (equal? player 'white) (car position) (cdr position)))

; get a player-piece from a position
(define (position-ref position location)
  (define black-at-location (position-player-ref (position-black position) location))
  (define white-at-location (position-player-ref (position-white position) location))
  (cond
    [(not (null? black-at-location)) (cons 'black black-at-location)]
    [(not (null? white-at-location)) (cons 'white white-at-location)]
    [true null]))

; get a piece from a player-position
(define (position-player-ref position-player location)
  (define piece-at-location (filter (λ (pl) (equal? (cdr pl) location)) position-player))
  (if (empty? piece-at-location)
    null
    (car (first piece-at-location))))

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

; returns all offsets that can be created from transformations of the given offset
(define (all-offsets offset)
  (define (all-transformations offset)
    (match offset [(cons rank file)
      (list
        (cons rank file)
        (cons (- rank) file)
        (cons rank (- file))
        (cons (- rank) (- file))
        (cons file rank)
        (cons (- file) rank)
        (cons file (- rank))
        (cons (- file) (- rank)))]))
  (remove-duplicates (all-transformations offset)))

; returns the moves a leaper can make
(define (leaper-moves offset position source)
  (filter in-bounds
    (map (curry add-location source) (all-offsets offset))))

; returns all moves a rider can make along a single path
(define (rider-path offset position source)
  (define dest (add-location source offset))
  (define piece-at-dest (position-ref position dest))
  (cond
    [(not (in-bounds dest)) null]
    [(null? piece-at-dest) (cons dest (rider-path offset position dest))]
    [true (list dest)]))

(define (rider-moves offset position source)
  (append*
    (map (lambda (direction) (rider-path direction position source))
      (all-offsets offset))))

; defines piece moves in terms of leapers and riders
(define knight-moves (curry leaper-moves '(1 . 2)))
(define rook-moves (curry rider-moves '(1 . 0)))
(define bishop-moves (curry rider-moves '(1 . 1)))
(define (queen-moves position source)
  (append
    (rider-moves '(1 . 0) position source)
    (rider-moves '(1 . 1) position source)))
(define (king-moves position source)
  (append
    (leaper-moves '(1 . 0) position source)
    (leaper-moves '(1 . 1) position source)))

(print-grid (new-grid))

(rook-moves (new-position) '(3 . 3))


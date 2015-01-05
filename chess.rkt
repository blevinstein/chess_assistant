#lang racket

; informal types
; location : (file . rank)
; color-piece : (color . piece)
; piece-location : (piece . location)
; grid : ( ( color-piece+ )+ )
;        ( row+ )
; position: ( ( piece-location+ ) . ( piece-location+ ) )
;           ( white-piece-locations . black-piece-locations )
; move : (color piece source dest)

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

; returns a - b using character codes
(define (char-diff a b) (- (char->integer a) (char->integer b)))

; gives the number representing a rank
(define (rank-string rank)
  (number->string (+ rank 1)))

; returns true if a given character represents a rank
(define (rank? c) (and (char>=? c #\a) (char<=? c #\h)))

; converts ranks a-h into indices 0-7
(define (char->rank c)
  (when (not (rank? c)) (raise "out of bounds"))
  (char-diff c #\a))

; returns true if a given character represents a file
(define (file? c) (and (char>=? c #\1) (char<=? c #\8)))

; converts files 1-8 into indices 0-7
(define (char->file c)
  (when (not (file? c)) (raise "out of bounds"))
  (char-diff c #\1))

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

; creates a new location from a string representation
(define (new-location str)
  (cons (char->rank (string-ref str 0))
        (char->file (string-ref str 1))))

; location members
(define location-file car)
(define location-rank cdr)

; move : (color piece source destination)
(define (new-move position color str)
  (match (string->list str)
    [(list p r f) null]))

; TODO
;(define (try-move position color piece location)
;  (define color-pieces (position-player position color))
;  (define pieces (filter (lambda (piece-location) (equal? piece (car piece-location)))))

; move members
(define move-color first)
(define move-piece second)
(define move-source third)
(define move-dest fourth)

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

; defines the pieces on the back row
(define back-row
  '(R N B Q K B N R))

; create a new grid
(define (new-grid)
  (append
    (list (map (λ (piece) (cons 'white piece)) back-row))
    (list (build-list 8 (λ (i) (cons 'white 'P))))
    (build-list 4 (λ (i) (build-list 8 (λ (i) null))))
    (list (build-list 8 (λ (i) (cons 'black 'P))))
    (list (map (λ (piece) (cons 'black piece)) back-row))))

; get a color-piece from a grid
(define (grid-ref grid location)
  (match location [(cons file rank)
    (list-ref (list-ref grid rank) file)]))

; print subroutines

(define (print-square background color-piece)
  (display (if (equal? background 'black) bg-black bg-white))
  (display " ")
  (match color-piece
    [(cons color piece)
      (display (if (equal? color 'black) fg-black fg-white))
      (display (piece-code piece))]
    [null (display " ")])
  (display " "))

(define (print-grid grid)
  (display "  ")
  (for ([file (in-range 8)])
    (display (string-append " " (file-string file) " ")))
  (displayln "")
  (for ([rank (in-range 8)])
    (display (string-append (rank-string rank) " "))
    (for ([file (in-range 8)])
      (define color-piece (grid-ref grid (cons file rank)))
      (print-square (if (equal? (modulo (+ file rank) 2) 0) 'black 'white) color-piece))
    (displayln reset)))

(define (color-piece-repr color-piece)
  (match color-piece
    [(cons color piece) (string-append (symbol->string color) " " (symbol->string piece))]
    [_ "\u2205"]))

(define (piece-location-repr piece-location)
  (match piece-location
    [(cons piece location) (string-append (piece-code piece) " @ " (location-repr location))]))

(define (location-repr location)
  (match location
    [(cons file rank) (string-append (file-string file) (rank-string rank))]))

; grid->position conversion code

(define (grid->position grid)
  (define (grid-player-position grid player)
    (filter (compose1 not null?)
      (for*/list ([rank (in-range 8)] [file (in-range 8)])
        (define player-piece (grid-ref grid (cons file rank)))
        (match player-piece 
          [(cons color piece)
          (if (equal? color player)
              (cons piece (cons file rank))
              null)]
          [_ null]))))
  (cons
    (grid-player-position grid 'white)
    (grid-player-position grid 'black)))


; create a new position
(define (new-position) (grid->position (new-grid)))

; position members
(define position-white car)
(define position-black cdr)

; gets the position of a specified player
(define (position-player position color)
  (if (equal? color 'white) (position-white position) (position-black position)))

; get a color-piece from a position
(define (position-ref position location)
  (define black-at-location (position-player-ref (position-black position) location))
  (define white-at-location (position-player-ref (position-white position) location))
  (cond
    [(not (null? black-at-location)) (cons 'black black-at-location)]
    [(not (null? white-at-location)) (cons 'white white-at-location)]
    [else null]))

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
    (+ (location-file a) (location-file b))
    (+ (location-rank a) (location-rank b))))

; checks that a location is in [0,8) x [0,8)
(define (in-bounds location)
  (match location
    [(cons file rank) (and
      (positive? rank) (< rank 8)
      (positive? file) (< file 8))]))

; returns all offsets that can be created from transformations of the given offset
(define (all-offsets offset)
  (define (all-transformations offset)
    (match offset [(cons file rank)
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
    [else (list dest)]))

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
(define (pawn-moves position source)
  (define rank (location-rank source))
  (define color (car (position-ref position source)))
  (cond
    [(and (equal? color 'white) (equal? rank 1))
      (list (add-location source '(0 . 1)) (add-location source '(0 . 2)))]
    [(and (equal? color 'black) (equal? rank 6))
      (list (add-location source '(0 . -1)) (add-location source '(0 . -2)))]
    [(equal? color 'white)
      (list (add-location source '(0 . 1)))]
    [(equal? color 'black)
      (list (add-location source '(0 . -1)))]
    [else (raise)]))

; returns the appropriate function for calculating a piece's moves
(define (piece-move-func piece)
  (hash-ref (hash
    'P pawn-moves
    'N knight-moves
    'R rook-moves
    'B bishop-moves
    'K king-moves
    'Q queen-moves)
  piece))

; returns possible moves, given a source location
(define (possible-moves position location)
  (define color-piece (position-ref position location))
  (when (null? color-piece) (raise "no piece there"))
  (define move-func (piece-move-func (cdr color-piece)))
  (move-func position location))

(print-grid (new-grid))

(define current-position (new-position))

(define (repl)
  (let loop ()
    (display "> ")
    (define input (read-line))
    (when (eof-object? input) (exit))
    (define input-location (new-location input))
    (displayln (map location-repr (possible-moves current-position input-location)))
    ; TODO given a move, make the move
    (loop)))

(repl)


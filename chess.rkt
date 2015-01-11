#lang racket

; informal types
; location : (file . rank)
; color-piece : (color . piece)
; piece-location : (piece . location)
; grid : ( ( color-piece+ )+ )
;        ( row+ )
; position : ( ( color piece location )+ )
; move : (source dest)

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
(define (rank? c) (and (char>=? c #\1) (char<=? c #\8)))

; converts ranks a-h into indices 0-7
(define (char->rank c)
  (when (not (rank? c)) (raise "rank out of bounds"))
  (char-diff c #\1))

; returns true if a given character represents a file
(define (file? c) (and (char>=? c #\a) (char<=? c #\h)))

; converts files 1-8 into indices 0-7
(define (char->file c)
  (when (not (file? c)) (raise "file out of bounds"))
  (char-diff c #\a))

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
  (cons (char->file (string-ref str 0))
        (char->rank (string-ref str 1))))

; location members
(define location-file car)
(define location-rank cdr)

; creates a new move from a string representation
(define (new-move position color str)
  (define (infer-move piece dest
      #:hint [hint empty]
      #:promote [promote empty]
      #:castle [castle empty])
    ; find all pieces of the right color and type
    (define candidate-locations
      (map (lambda (cpl) (third cpl))
        (filter (lambda (cpl) (and (equal? color (first cpl)) (equal? piece (second cpl))))
          position)))
    ; find all locations which are the source of a valid move to dest
    (define valid-locations (filter
      (lambda (location)
        (and (valid-move position (cons location dest))
             (or (null? hint) (hint location))))
      candidate-locations))
    (cons (cond
      [(equal? 1 (length valid-locations)) (first valid-locations)]
      [(equal? 0 (length valid-locations)) (raise "no valid moves found")]
      [else (raise "not implemented yet")]) dest))
  (define (parse-loc file rank) (cons (char->file file) (char->rank rank)))
  (match (string->list str)
    [(list file rank)
      (infer-move 'P (parse-loc file rank))]
    [(list piece file rank) #:when (is-piece? piece)
      (infer-move (char->piece piece) (parse-loc file rank))]
    [(list hint file rank) #:when (is-hint? (hint-pred hint))
      (infer-move 'P (parse-loc file rank) #:hint (hint-pred hint))]
    [(list piece hint file rank) #:when (and (is-piece? piece) (is-hint? hint))
      (infer-move (char->piece piece) (parse-loc file rank) #:hint (hint-pred hint))]
    [(list file rank promote) #:when (is-piece? promote)
      (infer-move 'P (parse-loc file rank) #:promote (char->piece promote))]
    [(list hint file rank promote) #:when (and (is-hint? hint) (is-piece? promote))
      (infer-move 'P (parse-loc file rank)
        #:hint (hint-pred hint) #:promote (char->piece promote))]
    [(list piece hfile hrank file rank)
        #:when (and (file? hfile) (rank? hrank) (is-piece? piece))
      (infer-move (char->piece piece) (parse-loc file rank)
          #:hint (curry equal? (parse-loc hfile hrank)))]
    [_ (raise "unrecognized move")]))

; creates a predicate which acts on locations, given a hint character (1-8 or a-h)
(define (hint-pred hint-char)
  (cond
    [(file? hint-char)
      (lambda (location) (equal? (location-file location) (char->file hint-char)))]
    [(rank? hint-char)
      (lambda (location) (equal? (location-rank location) (char->rank hint-char)))]
    [else (raise "not a valid hint")]))

(define (is-hint? hint-char)
  (or (file? hint-char) (rank? hint-char)))

; returns true if a chracter represents a valid piece
(define (is-piece? piece-char)
  (member piece-char (string->list "KQNBRP")))

(define (char->piece c)
  (when (not (is-piece? c)) raise "not a valid piece")
  (string->symbol (list->string (list c))))

; move members
(define move-source car)
(define move-dest cdr)

; used to generate terminal escape sequences
(define (esc . codes)
  (string-append
    "\e["
    (string-join (map number->string codes) ";")
    "m"))

(define (other-player color)
  (match color
    ['black 'white]
    ['white 'black]
    [else raise "invalid argument"]))

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

(define (print-position position)
  (print-grid (position->grid position)))

(define (print-locations locations)
  (displayln (map location-repr locations)))

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
  (filter (compose1 not null?)
    (for*/list ([rank (in-range 8)] [file (in-range 8)])
      (define location (cons file rank))
      (define player-piece (grid-ref grid location))
      (match player-piece 
        [(cons color piece) (list color piece location)]
        [_ null]))))

(define (position->grid position)
  (for/list ([rank (in-range 8)])
    (for/list ([file (in-range 8)])
      (position-ref position (cons file rank)))))

; create a new position
(define (new-position) (grid->position (new-grid)))

; get a color-piece from a position
(define (position-ref position location)
  (define (at-location? cpl) (equal? (third cpl) location))
  (define pieces-at-location (filter at-location? position))
  (when (> (length pieces-at-location) 1) (raise "Invalid state!"))
  (if (empty? pieces-at-location)
    null
    (match pieces-at-location [(list (list color piece _)) (cons color piece)])))

; adds two locations together like vectors
(define (add-location . locations)
  (for/fold ([sum '( 0 . 0 )]) ([loc locations])
    (cons
      (+ (location-file loc) (location-file sum))
      (+ (location-rank loc) (location-rank sum)))))

; checks that a location is in [0,8) x [0,8)
(define (in-bounds location)
  (match location
    [(cons file rank) (and
      (>= rank 0) (< rank 8)
      (>= file 0) (< file 8))]))

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
  (map (lambda (dest) (cons source dest))
    (filter in-bounds
      (map (curry add-location source) (all-offsets offset)))))

; returns all moves a rider can make along a single path
(define (rider-path offset position original source)
  (define dest (add-location source offset))
  (define move (cons original dest))
  (define piece-at-dest (position-ref position dest))
  (cond
    [(not (in-bounds dest)) null]
    [(null? piece-at-dest) (cons move (rider-path offset position original dest))]
    [else (list move)]))

(define (rider-moves offset position source)
  (append*
    (map (lambda (direction) (rider-path direction position source source))
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
  (define (open? space) (null? (position-ref position space)))
  (define color (car (position-ref position source)))
  (define (can-attack? space)
    (define color-piece (position-ref position space))
    (and (not (null? color-piece)) (not (equal? (car color-piece) color))))
  (define rank (location-rank source))
  (define direction (if (equal? color 'white) '(0 . 1) '(0 . -1)))
  (define unmoved (or (and (equal? color 'white) (equal? rank 1))
                      (and (equal? color 'black) (equal? rank 6))))
  (define (move-to space) (cons source space))
  (define plus-one (add-location source direction))
  (define plus-two (add-location source direction direction))
  (define left (add-location source direction '(-1 . 0)))
  (define right (add-location source direction '(1 . 0)))
  (filter (compose1 not null?)
    (list
      (if (open? plus-one) (move-to plus-one) empty)
      (if (and unmoved (open? plus-one) (open? plus-two)) (move-to plus-two) empty)
      (if (can-attack? left) (move-to left) empty)
      (if (can-attack? right) (move-to right) empty))))

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

; makes a move and returns the new position
; NOTE does not check if the move is valid
; NOTE does not support en passant
(define (make-move position move)
  (define source (move-source move))
  (define dest (move-dest move))
  (define color-piece (position-ref position source))
  (define piece (cdr color-piece))
  (define (at-location? loc cpl) (equal? (third cpl) loc))
  ; remove pieces from source and dest
  (define to-remove-list (append (filter (curry at-location? source) position)
                                 (filter (curry at-location? dest) position)))
  ; add piece from source to dest
  (define to-add (list (car color-piece) (cdr color-piece) dest))
  (list* to-add
    (remove* to-remove-list
      position)))

; get all moves from a source in source-list to a dest in dest-list
; NOTE does not handle castling
(define (get-moves position source-list dest-list)
  (filter
    (curry valid-move position)
    (for*/list ([source source-list] [dest dest-list]) (cons source dest))))

(define (valid-move position move)
  (define source-color-piece (position-ref position (move-source move)))
  (define dest-color-piece (position-ref position (move-dest move)))
  (and
    ; source contains a piece
    (not (null? source-color-piece))
    ; this move can be performed by the source piece
    (member move (possible-moves position (move-source move)))
    ; dest is empty or contains an enemy piece
    (or
      (null? dest-color-piece)
      (and
        (not (null? dest-color-piece))
        (not (equal? (car source-color-piece) (car dest-color-piece)))))))

; return a list of enemy squares attacking a given square
(define (attackers position target)
  (define target-color (car (position-ref position target)))
  (define enemy-locations
    (map (lambda (cpl) (third cpl))
      (filter (lambda (cpl) (equal? (first cpl) (other-player target-color)))
        position)))
  (map car (get-moves position enemy-locations (list target))))

; return a list of friendly squares defending a given square
(define (defenders position target)
  (define target-color (car (position-ref position target)))
  (define friendly-locations
    (map (lambda (cpl) (third cpl))
      (filter (lambda (cpl) (equal? (first cpl) target-color))
        position)))
  (map car (get-moves position friendly-locations (list target))))

(define (threat-count position location)
  (-
    (length (defenders position location))
    (length (attackers position location))))

; EXPERIMENTAL code below

; TODO add unit tests
;
; TODO move-repr
;
; TODO castling
; TODO add REPL commands
; TODO store history of moves, allow replay/undo
;
; TODO en passant
; TODO check, checkmate
; TODO draws
;
; TODO catch errors in repl
; TODO rider-shadow (for pins/skewers)

(define errormsg (string-append (esc 31) "[ERROR]" (esc 0)))

(define (repl)
  (define current-position (new-position))
  (define to-move 'white)

  (let loop ()
    (define (read-line-exit)
      (define line (read-line))
      (when (eof-object? line) (exit))
      line)

    (print-position current-position)
    
    (displayln (string-append "to move: " (symbol->string to-move)))

    ;(display "attackers ")
    ;(print-locations (attackers current-position source))
    ;(display "defenders ")
    ;(print-locations (defenders current-position source))
    ;(display "threat # ")
    ;(displayln (threat-count current-position source))

    (with-handlers ([string? (lambda (exn) (displayln errormsg))])
      (display "move > ")
      (define move (new-move current-position to-move (read-line-exit)))
      (define source (move-source move))
      (define dest (move-dest move))

      (define move-color (car (position-ref current-position source)))
      (if (equal? move-color to-move)
        (if (valid-move current-position move)
          (list
            (set! current-position (make-move current-position move))
            (set! to-move (other-player to-move)))
          (displayln "Invalid move!"))
        (displayln "Wrong player!")))

    (loop)))

(repl)


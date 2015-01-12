#lang typed/racket

(provide (struct-out location))
(struct: location ([file : Integer] [rank : Integer]) #:transparent)

(define-type ColorPiece (Pair Symbol Symbol))

(define-type PieceLocation (Pair Symbol location))

(define-type ColorPieceLocation (List Symbol Symbol location))

(define-type Grid (Listof (Listof ColorPiece)))

(define-type Position (Listof ColorPieceLocation))

(define-type Move (Pair location location))

; TODO refactor print code
; http://docs.racket-lang.org/reference/Printer_Extension.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._gen~3acustom-write%29%29

; gives the point value of each piece
(provide piece-value)
(: piece-value (-> Symbol Integer))
(define (piece-value piece)
  (hash-ref #hash(
    (P . 1)
    (N . 3)
    (B . 3)
    (R . 5)
    (Q . 9)
    (K . 0)) piece))

; gives the unicode character of each piece
(provide piece-code)
(: piece-code (-> Symbol String))
(define (piece-code piece)
  (hash-ref #hash(
    (K . "\u265a")
    (Q . "\u265b")
    (R . "\u265c")
    (B . "\u265d")
    (N . "\u265e")
    (P . "\u265f"))
    piece))

; returns a - b using character codes
(provide char-diff)
(: char-diff (-> Char Char Integer))
(define (char-diff a b) (- (char->integer a) (char->integer b)))

; gives the number representing a rank
(provide rank-string)
(: rank-string (-> Integer String))
(define (rank-string rank)
  (number->string (+ rank 1)))

; returns true if a given character represents a rank
(provide rank?)
(: rank? (-> Char Boolean))
(define (rank? c) (and (char>=? c #\1) (char<=? c #\8)))

; converts ranks a-h into indices 0-7
(provide char->rank)
(: char->rank (-> Char Integer))
(define (char->rank c)
  (when (not (rank? c)) (raise "rank out of bounds"))
  (char-diff c #\1))

; returns true if a given character represents a file
(provide file?)
(: file? (-> Char Boolean))
(define (file? c) (and (char>=? c #\a) (char<=? c #\h)))

; converts files 1-8 into indices 0-7
(provide char->file)
(: char->file (-> Char Integer))
(define (char->file c)
  (when (not (file? c)) (raise "file out of bounds"))
  (char-diff c #\a))

; gives the letter representing a file
(provide file-string)
(: file-string (-> Integer String))
(define (file-string file)
  (hash-ref #hash(
    (0 . "a")
    (1 . "b")
    (2 . "c")
    (3 . "d")
    (4 . "e")
    (5 . "f")
    (6 . "g")
    (7 . "h"))
    file))

(provide new-location)
(: new-location (-> String location))
(define (new-location str)
  (location (char->file (string-ref str 0))
            (char->rank (string-ref str 1))))

; creates a new move from a string representation
(provide new-move)
(: new-move (-> Position Symbol String Move))
(define (new-move position color str)
  ; creates a predicate which acts on locations, given a hint character (1-8 or a-h)
  (: hint-pred (-> Char (-> location Boolean)))
  (define (hint-pred hint-char)
    (cond
      [(file? hint-char)
        (lambda (loc) (equal? (location-file loc) (char->file hint-char)))]
      [(rank? hint-char)
        (lambda (loc) (equal? (location-rank loc) (char->rank hint-char)))]
      [else (raise "not a valid hint")]))
  (: is-hint? (-> Char Boolean))
  (define (is-hint? hint-char)
    (or (file? hint-char) (rank? hint-char)))
  ; returns true if a chracter represents a valid piece
  (: is-piece? (-> Char Boolean))
  (define (is-piece? piece-char)
    (not (null? (member piece-char (string->list "KQNBRP")))))
  (: char->piece (-> Char Symbol))
  (define (char->piece c)
    (when (not (is-piece? c)) raise "not a valid piece")
    (string->symbol (list->string (list c))))
  (: infer-move (-> Symbol location
                    [#:hint (-> location Boolean)]
                    [#:promote Symbol]
                    [#:castle Symbol] Move))
  (define (infer-move piece dest
      #:hint [hint empty]
      #:promote [promote empty]
      #:castle [castle empty])
    ; find all pieces of the right color and type
    (define candidate-locations
      (map (lambda: ([cpl : ColorPieceLocation]) (third cpl))
        (filter (lambda: ([cpl : ColorPieceLocation]) (and (equal? color (first cpl)) (equal? piece (second cpl))))
          position)))
    ; find all locations which are the source of a valid move to dest
    (define valid-locations (filter
      (lambda: ([loc : location])
        (and (valid-move position (cons loc dest))
             (or (null? hint) (hint loc))))
      candidate-locations))
    (cons (cond
      [(equal? 1 (length valid-locations)) (first valid-locations)]
      [(equal? 0 (length valid-locations)) (raise "no valid moves found")]
      [else (raise "not implemented yet")]) dest))
  (: parse-loc (-> Char Char location))
  (define (parse-loc file rank) (location (char->file file) (char->rank rank)))
  (match (string->list str)
    [(list file rank)
      (infer-move 'P (parse-loc file rank))]
    [(list piece file rank) #:when (is-piece? piece)
      (infer-move (char->piece piece) (parse-loc file rank))]
    ; TODO fix
    ;[(list hint file rank) #:when (is-hint? (hint-pred hint))
    ;  (infer-move 'P (parse-loc file rank) #:hint (hint-pred hint))]
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

(provide valid-move)
(: valid-move (-> Position Move Boolean))
(define (valid-move position move)
  ; TODO fix
  (define source-color-piece (position-ref position (move-source move)))
  (define dest-color-piece (position-ref position (move-dest move)))
  (cond
    [(and (color-piece? source-color-piece) (color-piece? dest-color-piece))
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
            (not (equal? (car source-color-piece) (car dest-color-piece))))))]
    [else false]))

; get a color-piece from a position
(provide position-ref)
(: position-ref (-> Position location (Option ColorPiece)))
(define (position-ref position loc)
  (: at-location? (-> ColorPieceLocation Boolean))
  (define (at-location? cpl) (equal? (third cpl) loc))
  (define pieces-at-location (filter at-location? position))
  (when (> (length pieces-at-location) 1) (raise "Invalid state!"))
  (if (empty? pieces-at-location)
    null
    (match pieces-at-location [(list (list color piece _)) (cons color piece)])))

; move members
(provide move-source move-dest)
(: move-source (-> Move location))
(define move-source car)
(: move-dest (-> Move location))
(define move-dest cdr)

; returns possible moves, given a source location
(provide possible-moves)
(: possible-moves (-> Position location (Listof Move)))
(define (possible-moves position loc)
  (define color-piece (position-ref position loc))
  (when (null? color-piece) (raise "no piece there"))
  (define move-func (piece-move-func (cdr color-piece)))
  (move-func position loc))

; returns the appropriate function for calculating a piece's moves
(: piece-move-func (-> Symbol (-> Position location (Listof Move))))
(define (piece-move-func piece)
  (hash-ref (hash
    'P pawn-moves
    'N knight-moves
    'R rook-moves
    'B bishop-moves
    'K king-moves
    'Q queen-moves)
  piece))

; returns the moves a leaper can make
(: leaper-moves (-> location Position location (Listof Move)))
(define (leaper-moves offset position source)
  (map (lambda (dest) (cons source dest))
    (filter in-bounds
      (map (curry add-location source) (all-offsets offset)))))

; returns all moves a rider can make along a single path
(: rider-path (-> location Position location location (Listof Move)))
(define (rider-path offset position original source)
  (define dest (add-location source offset))
  (define move (cons original dest))
  (define piece-at-dest (position-ref position dest))
  (cond
    [(not (in-bounds dest)) null]
    [(null? piece-at-dest) (cons move (rider-path offset position original dest))]
    [else (list move)]))

(: rider-moves (-> location Position location (Listof Move)))
(define (rider-moves offset position source)
  (append*
    (map (lambda (direction) (rider-path direction position source source))
      (all-offsets offset))))

; defines piece moves in terms of leapers and riders
(: knight-moves (-> Position location (Listof Move)))
(define knight-moves (curry leaper-moves (location 1 2)))
(: rook-moves (-> Position location (Listof Move)))
(define rook-moves (curry rider-moves (location 1 0)))
(: bishop-moves (-> Position location (Listof Move)))
(define bishop-moves (curry rider-moves (location 1 1)))
(: queen-moves (-> Position location (Listof Move)))
(define (queen-moves position source)
  (append
    (rider-moves (location 1 0) position source)
    (rider-moves (location 1 1) position source)))
(: king-moves (-> Position location (Listof Move)))
(define (king-moves position source)
  (append
    (leaper-moves (location 1 0) position source)
    (leaper-moves (location 1 1) position source)))
(: pawn-moves (-> Position location (Listof Move)))
(define (pawn-moves position source)
  (define (open? space) (null? (position-ref position space)))
  (define color (car (position-ref position source)))
  (define (can-attack? space)
    (define color-piece (position-ref position space))
    (and (not (null? color-piece)) (not (equal? (car color-piece) color))))
  (define rank (location-rank source))
  (define direction (if (equal? color 'white) (location 0 1) (location 0 -1)))
  (define unmoved (or (and (equal? color 'white) (equal? rank 1))
                      (and (equal? color 'black) (equal? rank 6))))
  (define (move-to space) (cons source space))
  (define plus-one (add-location source direction))
  (define plus-two (add-location source direction direction))
  (define left (add-location source direction (location -1 0)))
  (define right (add-location source direction (location 1 0)))
  (filter (compose1 not null?)
    (list
      (if (open? plus-one) (move-to plus-one) empty)
      (if (and unmoved (open? plus-one) (open? plus-two)) (move-to plus-two) empty)
      (if (can-attack? left) (move-to left) empty)
      (if (can-attack? right) (move-to right) empty))))

; checks that a location is in [0,8) x [0,8)
(define (in-bounds loc)
  (match loc
    [(location file rank) (and
      (>= rank 0) (< rank 8)
      (>= file 0) (< file 8))]))

; adds two locations together like vectors
; TODO refactor
(provide add-location)
(: add-location (-> (Listof location) location))
(define (add-location . locations)
  (for/fold ([sum (location 0  0 )]) ([loc locations])
    (location
      (+ (location-file loc) (location-file sum))
      (+ (location-rank loc) (location-rank sum)))))

; create a new position
(provide new-position)
(define (new-position) (grid->position (new-grid)))

; returns all offsets that can be created from transformations of the given offset
(define (all-offsets offset)
  (define (all-transformations offset)
    (match offset [(location file rank)
      (list
        (location rank file)
        (location (- rank) file)
        (location rank (- file))
        (location (- rank) (- file))
        (location file rank)
        (location (- file) rank)
        (location file (- rank))
        (location (- file) (- rank)))]))
  (remove-duplicates (all-transformations offset)))

; grid->position conversion code

(: all-locations (Listof location))
(define all-locations
  (flatten
    (ann (for/list ([rank (in-range 8)])
      (ann (for/list ([file (in-range 8)])
        (location file rank)
      ) (Listof location))
    ) (Listof (Listof location)))))

(provide grid->position)
(: grid->position (-> Grid Position))
(define (grid->position grid)
  (define not-null (compose1 not null?))
  (define (custom-ref grid loc)
    (match (grid-ref grid loc)
      [(cons color piece) (list color piece loc)]
      [_ null]))
  (define filter-not-null (curry filter not-null))
  (define (piece-at loc)
    (grid-ref grid loc))
  (ann (filter-not-null
    (ann (for/list ([loc all-locations])
      (define player-piece (grid-ref grid loc))
      (ann (match player-piece 
        [(cons color piece) (list color piece loc)]
        [_ null]
      ) (Option ColorPieceLocation))
    ) (Listof (Option ColorPieceLocation)))
  ) Position))

(provide position->grid)
(: position->grid (-> Position Grid))
(define (position->grid position)
  (for/list ([rank (in-range 8)])
    (define: row : (Listof ColorPiece)
      (for/list ([file (in-range 8)])
        (position-ref position (location file rank))))
    row))

; makes a move and returns the new position
; NOTE does not check if the move is valid
; NOTE does not support en passant
(provide make-move)
(: make-move (-> Position location Position))
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
(: get-moves (-> Position (Listof location) (Listof location) (Listof Move)))
(define (get-moves position source-list dest-list)
  (filter
    (curry valid-move position)
    (flatten
      (ann (for/list ([source source-list])
        (ann (for/list ([dest dest-list])
          (cons source dest)
        ) (Listof Move))
      ) (Listof (Listof Move))))))

; return a list of enemy squares attacking a given square
(: attackers (-> Position location (Listof location)))
(define (attackers position target)
  (define target-color (car (position-ref position target)))
  (define enemy-locations
    (map (lambda (cpl) (third cpl))
      (filter (lambda (cpl) (equal? (first cpl) (other-player target-color)))
        position)))
  (map car (get-moves position enemy-locations (list target))))

; return a list of friendly squares defending a given square
(: defenders (-> Position location (Listof location)))
(define (defenders position target)
  (define target-color (car (position-ref position target)))
  (define friendly-locations
    (map (lambda (cpl) (third cpl))
      (filter (lambda (cpl) (equal? (first cpl) target-color))
        position)))
  (map car (get-moves position friendly-locations (list target))))

(: threat-count (-> Position location Integer))
(define (threat-count position loc)
  (-
    (length (defenders position loc))
    (length (attackers position loc))))

; defines the pieces on the back row
(: back-row (-> (Listof Symbol)))
(define back-row
  '(R N B Q K B N R))

; create a new grid
(: new-grid (-> Grid))
(define (new-grid)
  (append
    (list (map (λ (piece) (cons 'white piece)) back-row)))
    (list (build-list 8 (λ (i) (cons 'white 'P))))
    (build-list 4 (λ (i) (build-list 8 (λ (i) null))))
    (list (build-list 8 (λ (i) (cons 'black 'P))))
    (list (map (λ (piece) (cons 'black piece)) back-row)))

; get a color-piece from a grid
(provide grid-ref)
(: grid-ref (-> Grid location ColorPiece))
(define (grid-ref grid loc)
  (match loc [(location file rank)
    (list-ref (list-ref grid rank) file)]))

(provide other-player)
(: other-player (-> Symbol Symbol))
(define (other-player color)
  (match color
    ['black 'white]
    ['white 'black]
    [else raise 'invalid-argument]))

(provide color-piece-repr)
(: color-piece-repr (-> ColorPiece String))
(define (color-piece-repr color-piece)
  (match color-piece
    [(cons color piece) (string-append (symbol->string color) " " (symbol->string piece))]
    [_ "\u2205"]))

(provide piece-location-repr)
(: piece-location-repr (-> PieceLocation String))
(define (piece-location-repr piece-location)
  (match piece-location
    [(cons piece loc) (string-append (piece-code piece) " @ " (location-repr loc))]))

(provide location-repr)
(: location-repr (-> location String))
(define (location-repr loc)
  (match loc
    [(location file rank) (string-append (file-string file) (rank-string rank))]))

(provide move-repr)
(: move-repr (-> Move String))
(define (move-repr move)
  (string-append (location-repr (move-source move))
                 "->"
                 (location-repr (move-dest move))))

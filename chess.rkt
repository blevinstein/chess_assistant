#lang typed/racket

(provide (struct-out location))
(struct: location ([file : Integer] [rank : Integer]) #:transparent)

(define-type ColorPiece (Pair Symbol Symbol))
(define-predicate color-piece? ColorPiece) 
(define-type PieceLocation (Pair Symbol location))
(define-predicate piece-location? PieceLocation)

(define-type ColorPieceLocation (List Symbol Symbol location))
(define-predicate color-piece-location? ColorPieceLocation)

(define-type Grid (Listof (Listof (Option ColorPiece))))

(define-type Position (Listof ColorPieceLocation))

(define-type Move (Listof move))
(provide (struct-out move))
(struct: move ([source : location] [dest : location]) #:transparent)

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
  (when (not (rank? c)) (raise 'rank-out-of-bounds))
  (char-diff c #\1))

; returns true if a given character represents a file
(provide file?)
(: file? (-> Char Boolean))
(define (file? c) (and (char>=? c #\a) (char<=? c #\h)))

; converts files 1-8 into indices 0-7
(provide char->file)
(: char->file (-> Char Integer))
(define (char->file c)
  (when (not (file? c)) (raise 'file-out-of-bounds))
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
; TODO promotions
; TODO castling
; TODO en passant
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
      [else (raise 'not-a-valid-hint)]))
  (: is-hint? (-> Char Boolean))
  (define (is-hint? hint-char)
    (or (file? hint-char) (rank? hint-char)))
  ; returns true if a chracter represents a valid piece
  (: is-piece? (-> Char Boolean))
  (define (is-piece? piece-char)
    (not (null? (member piece-char (string->list "KQNBRP")))))
  (: char->piece (-> Char Symbol))
  (define (char->piece c)
    (when (not (is-piece? c)) (raise 'not-a-valid-piece))
    (string->symbol (list->string (list c))))
  (: infer-move (-> Symbol location
                    [#:hint (-> location Boolean)]
                    [#:promote Symbol] move))
  (define (infer-move piece dest
      #:hint [hint empty]
      #:promote [promote empty])
    ; find all pieces of the right color and type
    (define candidate-locations
      (map (lambda: ([cpl : ColorPieceLocation]) (third cpl))
        (filter (lambda: ([cpl : ColorPieceLocation])
            (and (equal? color (first cpl))
                 (equal? piece (second cpl))))
          position)))
    ; find all locations which are the source of a valid move to dest
    (define valid-locations (filter
      (lambda: ([loc : location])
        (and (valid-move position (move loc dest))
             (or (null? hint) (hint loc))))
      candidate-locations))
    (move (cond
      [(equal? 1 (length valid-locations)) (first valid-locations)]
      [(equal? 0 (length valid-locations)) (raise 'no-valid-moves-found)]
      [else (raise 'not-implemented-yet)]) dest))
  (define (back-rank color)
    (match color ['white 0] ['black 7]))
  (: parse-loc (-> Char Char location))
  (define (parse-loc file rank) (location (char->file file) (char->rank rank)))
  (match (string->list str)
    ; kingside castle
    [(list #\O #\- #\O)
      (list (move (location (char->file #\h) (back-rank color)) (location (char->file #\f) (back-rank color)))
            (move (location (char->file #\e) (back-rank color)) (location (char->file #\g) (back-rank color))))]
    ; queenside castle
    [(list #\O #\- #\O #\- #\O)
      (list (move (location (char->file #\a) (back-rank color)) (location (char->file #\d) (back-rank color)))
            (move (location (char->file #\e) (back-rank color)) (location (char->file #\c) (back-rank color))))]
    [(list file rank)
      (list (infer-move 'P (parse-loc file rank)))]
    [(list piece file rank) #:when (is-piece? piece)
      (list (infer-move (char->piece piece) (parse-loc file rank)))]
    [(list hint file rank) #:when (is-hint? hint)
      (list (infer-move 'P (parse-loc file rank) #:hint (hint-pred hint)))]
    [(list piece hint file rank) #:when (and (is-piece? piece) (is-hint? hint))
      (list (infer-move (char->piece piece) (parse-loc file rank) #:hint (hint-pred hint)))]
    [(list file rank promote) #:when (is-piece? promote)
      (list (infer-move 'P (parse-loc file rank) #:promote (char->piece promote)))]
    [(list hint file rank promote) #:when (and (is-hint? hint) (is-piece? promote))
      (list (infer-move 'P (parse-loc file rank)
        #:hint (hint-pred hint) #:promote (char->piece promote)))]
    [(list piece hfile hrank file rank)
        #:when (and (file? hfile) (rank? hrank) (is-piece? piece))
      (list (infer-move (char->piece piece) (parse-loc file rank)
          #:hint (curry equal? (parse-loc hfile hrank))))]
    [_ (raise 'unrecognized-move)]))

(provide possible-move)
(: possible-move (-> Position move Boolean))
(define (possible-move position mv)
  (not (null? (member mv (possible-moves position (move-source mv))))))

; TODO allow castling
(provide valid-move)
(: valid-move (-> Position move Boolean))
(define (valid-move position mv)
  (define source (position-ref position (move-source mv)))
  (define dest (position-ref position (move-dest mv)))
  (define legal-moves (possible-moves position (move-source mv)))
  (cond
    ; source does not contain a piece
    [(false? source) #f]
    ; piece cannot make this move
    [(not (member mv legal-moves)) #f]
    ; cannot take own piece
    [(and (color-piece? source) (color-piece? dest) (equal? (car source) (car dest))) #f]
    [else #t]))

; get a ColorPiece from a position
(provide position-ref)
(: position-ref (-> Position location (Option ColorPiece)))
(define (position-ref position loc)
  (: at-location? (-> ColorPieceLocation Boolean))
  (define (at-location? cpl) (equal? (third cpl) loc))
  (define pieces-at-location (filter at-location? position))
  (when (> (length pieces-at-location) 1) (raise 'invalid-state))
  (if (empty? pieces-at-location)
    #f
    (match pieces-at-location [(list (list color piece _)) (cons color piece)])))

; like position-ref, but raises an exception if the location is empty
(provide position-ref!)
(: position-ref! (-> Position location ColorPiece))
(define (position-ref! position loc)
  (define optional-color-piece (position-ref position loc))
  (cond
    [(color-piece? optional-color-piece) optional-color-piece]
    [else (raise 'not-a-color-piece)]))

; returns possible moves, given a source location
(provide possible-moves)
(: possible-moves (-> Position location (Listof move)))
(define (possible-moves position loc)
  (define color-piece (position-ref position loc))
  (cond
    [(color-piece? color-piece)
      ((piece-move-func (cdr color-piece)) position loc)]
    [else (raise 'no-piece-at-location)]))

; returns the appropriate function for calculating a piece's moves
(: piece-move-func (-> Symbol (-> Position location (Listof move))))
(define (piece-move-func piece)
  (match piece
    ['P pawn-moves]
    ['N knight-moves]
    ['R rook-moves]
    ['B bishop-moves]
    ['K king-moves]
    ['Q queen-moves]
    [_ (raise 'not-a-valid-move)]))

; returns the moves a leaper can make
(: leaper-moves (-> location Position location (Listof move)))
(define (leaper-moves offset position source)
  (map (lambda: ([dest : location]) (move source dest))
    (filter in-bounds
      (map (curry add-location source) (all-offsets offset)))))

; returns all moves a rider can make along a single path
(: rider-path (-> location Position location location (Listof move)))
(define (rider-path offset position original source)
  (define dest (add-location source offset))
  (define mv (move original dest))
  (define piece-at-dest (position-ref position dest))
  (cond
    [(not (in-bounds dest)) null]
    [(false? piece-at-dest) (cons mv (rider-path offset position original dest))]
    [else (list mv)]))

(: rider-moves (-> location Position location (Listof move)))
(define (rider-moves offset position source)
  (append*
    (map (lambda: ([direction : location]) (rider-path direction position source source))
      (all-offsets offset))))

; defines piece moves in terms of leapers and riders

(provide knight-moves)
(: knight-moves (-> Position location (Listof move)))
(define knight-moves (curry leaper-moves (location 1 2)))

(provide rook-moves)
(: rook-moves (-> Position location (Listof move)))
(define rook-moves (curry rider-moves (location 1 0)))

(provide bishop-moves)
(: bishop-moves (-> Position location (Listof move)))
(define bishop-moves (curry rider-moves (location 1 1)))

(provide queen-moves)
(: queen-moves (-> Position location (Listof move)))
(define (queen-moves position source)
  (append
    (rider-moves (location 1 0) position source)
    (rider-moves (location 1 1) position source)))

(provide king-moves)
(: king-moves (-> Position location (Listof move)))
(define (king-moves position source)
  (append
    (leaper-moves (location 1 0) position source)
    (leaper-moves (location 1 1) position source)))

(provide pawn-moves)
(: pawn-moves (-> Position location (Listof move)))
(define (pawn-moves position source)
  (: open? (-> location Boolean))
  (define (open? space) (not (position-ref position space)))
  (define color (car (position-ref! position source)))
  (: can-attack? (-> location Boolean))
  (define (can-attack? space)
    (define color-piece (position-ref position space))
    (cond
      [(color-piece? color-piece) (not (equal? (car color-piece) color))]
      [else #f]))
  (define rank (location-rank source))
  (define direction (if (equal? color 'white) (location 0 1) (location 0 -1)))
  (define unmoved (or (and (equal? color 'white) (equal? rank 1))
                      (and (equal? color 'black) (equal? rank 6))))
  (: move-to (-> location move))
  (define (move-to space) (move source space))
  (define plus-one (add-location source direction))
  (define plus-two (add-location-list source direction direction))
  (define left (add-location-list source direction (location -1 0)))
  (define right (add-location-list source direction (location 1 0)))
  (define: optional-moves : (Listof (Option move))
    (list
      (if (open? plus-one) (move-to plus-one) #f)
      (if (and unmoved (open? plus-one) (open? plus-two)) (move-to plus-two) #f)
      (if (and (not (open? left)) (can-attack? left)) (move-to left) #f)
      (if (and (not (open? right)) (can-attack? right)) (move-to right) #f)))
  (filter move? optional-moves))

; checks that a location is in [0,8) x [0,8)
(define (in-bounds loc)
  (match loc
    [(location file rank) (and
      (>= rank 0) (< rank 8)
      (>= file 0) (< file 8))]))

; TODO combine with add-location
(provide add-location-list)
(: add-location-list (-> location * location))
(define (add-location-list . location-list)
  (for/fold ([sum (location 0 0)]) ([loc location-list])
    (add-location loc sum)))

; adds two locations together like vectors
(provide add-location)
(: add-location (-> location location location))
(define (add-location a b)
  (location
    (+ (location-file a) (location-file b))
    (+ (location-rank a) (location-rank b))))

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

(provide all-locations)
(: all-locations (Listof location))
(define all-locations
  (append*
    (ann (for/list ([rank (in-range 8)])
      (ann (for/list ([file (in-range 8)])
        (location file rank)
      ) (Listof location))
    ) (Listof (Listof location)))))

; grid->position conversion code

(provide grid->position)
(: grid->position (-> Grid Position))
(define (grid->position grid)
  (define cpls : (Listof (Option ColorPieceLocation))
    (for/list ([loc all-locations])
      (define color-piece (grid-ref grid loc))
      (ann (match color-piece
        [(cons color piece) (list color piece loc)]
        [_ #f]) (Option ColorPieceLocation))))
  (filter color-piece-location? cpls))

(provide position->grid)
(: position->grid (-> Position Grid))
(define (position->grid position)
  (for/list ([rank (in-range 8)])
    (ann (for/list ([file (in-range 8)])
      (position-ref position (location file rank))
      ) (Listof (Option ColorPiece)))))

; makes a move and returns the new position
; NOTE does not check if the move is valid
; NOTE does not support en passant
(provide make-move)
(: make-move (-> Position move Position))
(define (make-move position mv)
  (when (not (valid-move position mv)) (raise 'invalid-move-made))
  (define source (move-source mv))
  (define dest (move-dest mv))
  (define color-piece (position-ref! position source))
  (define piece (cdr color-piece))
  (: at-location? (-> location ColorPieceLocation Boolean))
  (define (at-location? loc cpl) (equal? (third cpl) loc))
  ; remove pieces from source and dest
  (define to-remove-list (append (filter (curry at-location? source) position)
                                 (filter (curry at-location? dest) position)))
  ; add piece from source to dest
  (: to-add ColorPieceLocation)
  (define to-add
    (list (car color-piece) (cdr color-piece) dest))
  (list* to-add
    (remove* to-remove-list
      position)))

; get all moves from a source in source-list to a dest in dest-list
(: get-moves (-> Position (Listof location) (Listof location) (Listof move)))
(define (get-moves position source-list dest-list)
  (filter
    (curry possible-move position)
    (append*
      (ann (for/list ([source source-list])
        (ann (for/list ([dest dest-list])
          (move source dest)
        ) (Listof move))
      ) (Listof (Listof move))))))

; return a list of enemy squares attacking a given square
(provide attackers)
(: attackers (-> Position location (Listof location)))
(define (attackers position target)
  (define target-color (car (position-ref! position target)))
  (define enemy-locations
    (map (lambda: ([cpl : ColorPieceLocation]) (third cpl))
      (filter
        (lambda: ([cpl : ColorPieceLocation]) (equal? (first cpl) (other-player target-color)))
        position)))
  (map move-source (get-moves position enemy-locations (list target))))

; return a list of friendly squares defending a given square
(provide defenders)
(: defenders (-> Position location (Listof location)))
(define (defenders position target)
  (define target-color (car (position-ref! position target)))
  (define friendly-locations
    (map (lambda: ([cpl : ColorPieceLocation]) (third cpl))
      (filter (lambda: ([cpl : ColorPieceLocation]) (equal? (first cpl) target-color))
        position)))
  (map move-source (get-moves position friendly-locations (list target))))

(provide threat-count)
(: threat-count (-> Position location Integer))
(define (threat-count position loc)
  (-
    (length (defenders position loc))
    (length (attackers position loc))))

; defines the pieces on the back row
(: back-row (Listof Symbol))
(define back-row
  '(R N B Q K B N R))

; create a new grid
(provide new-grid)
(: new-grid (-> Grid))
(define (new-grid)
  (append
    (list (map (lambda: ([piece : Symbol]) (cons 'white piece)) back-row))
    (list (build-list 8 (lambda: ([i : Integer]) (cons 'white 'P))))
    (build-list 4 (lambda: ([i : Integer]) (build-list 8 (lambda: ([i : Integer]) #f))))
    (list (build-list 8 (lambda: ([i : Integer]) (cons 'black 'P))))
    (list (map (lambda: ([piece : Symbol]) (cons 'black piece)) back-row))))

; get a ColorPiece from a grid
(provide grid-ref)
(: grid-ref (-> Grid location (Option ColorPiece)))
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
(: move-repr (-> move String))
(define (move-repr mv)
  (string-append (location-repr (move-source mv))
                 "->"
                 (location-repr (move-dest mv))))

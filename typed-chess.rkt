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

#lang racket

(require "chess.rkt")

; used to generate terminal escape sequences
(provide esc)
(define (esc . codes)
  (string-append
    "\e["
    (string-join (map number->string codes) ";")
    "m"))

; print subroutines

(provide print-square)
(define (print-square background color-piece)
  (display (if (equal? background 'black) bg-black bg-white))
  (display " ")
  (match color-piece
    [(cons color piece)
      (display (if (equal? color 'black) fg-black fg-white))
      (display (piece-code piece))]
    [null (display " ")])
  (display " "))

(provide print-grid)
(define (print-grid grid)
  (display "  ")
  (for ([file (in-range 8)])
    (display (string-append " " (file-string file) " ")))
  (displayln "")
  (for ([rank (in-range 8)])
    (display (string-append (rank-string rank) " "))
    (for ([file (in-range 8)])
      (define color-piece (grid-ref grid (location file rank)))
      (print-square (if (equal? (modulo (+ file rank) 2) 0) 'black 'white) color-piece))
    (displayln reset)))

(provide print-position)
(define (print-position position)
  (print-grid (position->grid position)))

(provide print-locations)
(define (print-locations locations)
  (displayln (map location-repr locations)))

(provide print-moves)
(define (print-moves moves)
  (displayln (map move-repr moves)))

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

; terminal escape sequences
(define bg-white (esc 48 5 246))
(define bg-black (esc 48 5 240))
(define fg-white (esc 97))
(define fg-black (esc 30))
(define reset (esc 0))

(define (info-cmd position input)
  (define square (new-location input))
  (print-moves (possible-moves position square)))

(define (command char)
  (hash-ref (hash
    #\I info-cmd)
    char))

(define (command? char)
  (member char (string->list "I")))

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

    (display "move > ")
    (define input (read-line-exit))
    (define first-char (string-ref input 0))
    (if (command? first-char)
      ((command first-char) current-position (substring input 1))
      (with-handlers ([string? (lambda (exn) (displayln errormsg))])
        (define move (new-move current-position to-move input))
        (define source (move-source move))
        (define dest (move-dest move))

        (define move-color (car (position-ref current-position source)))
        (if (equal? move-color to-move)
          (if (valid-move current-position move)
            (list
              (set! current-position (make-move current-position move))
              (set! to-move (other-player to-move)))
            (displayln "Invalid move!"))
          (displayln "Wrong player!"))))

    (loop)))

(repl)


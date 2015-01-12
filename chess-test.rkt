#lang racket

(require rackunit)
(require "chess.rkt")

(define chess-tests
  (test-suite
    "Chess Tests"

    (check-equal? (new-location "a1") (cons 0 0))
    (check-equal? (new-location "c4") (cons 2 3))
    (check-equal? (new-location "h8") (cons 7 7))

    (check-equal?
      (new-move (new-position) 'white "e4")
      (cons (new-location "e2") (new-location "e4")))

  ))

(require rackunit/text-ui)
(run-tests chess-tests)

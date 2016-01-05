#lang racket

(require json)
(require rackunit)
(require "chess.rkt")
(require "servlet.rkt")

(define chess-tests
  (test-suite
    "Json Tests"

    (test-case "location->json"
      (check-equal?
        (jsexpr->string (location->json (new-location "a1")))
        "[0,0]")
      (check-equal?
        (jsexpr->string (location->json (new-location "b3")))
        "[1,2]"))

    (test-case "move->json"
      (check-equal?
        (jsexpr->string (move->json (new-move (new-position) 'white "e4")))
        "[[[4,1],[4,3]]]"))

    (test-case "json->grid"
      (check-equal?
        (json->grid (grid->json (new-grid)))
        (new-grid)))
  ))

(require rackunit/text-ui)
(run-tests chess-tests)


#lang racket

(require rackunit)
(require "chess.rkt")
(require "servlet.rkt")

(define chess-tests
  (test-suite
    "Json Tests"

    (test-case "location->json"
      (check-equal?
        (location->json (new-location "a1"))
        (list 0 0)))
  ))

(require rackunit/text-ui)
(run-tests chess-tests)

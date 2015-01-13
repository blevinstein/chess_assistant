#lang racket

(require rackunit)
(require "chess.rkt")

(define chess-tests
  (test-suite
    "Chess Tests"

    (test-case "new-location"
      (check-equal? (new-location "a1") (location 0 0))
      (check-equal? (new-location "c4") (location 2 3))
      (check-equal? (new-location "h8") (location 7 7)))

    (test-case "new-grid"
      (check-equal? (length (new-grid)) 8)
      (check-equal? (length (list-ref (new-grid) 0)) 8))

    (test-case "grid-ref"
      (check-equal? (grid-ref (new-grid) (location 0 0)) (cons 'white 'R))
      (check-equal? (grid-ref (new-grid) (location 0 1)) (cons 'white 'P))
      (check-equal? (grid-ref (new-grid) (location 1 0)) (cons 'white 'N)))

    (test-case "new-position"
      (check-equal? (length (new-position)) 32))

    (test-case "new-move"
      (check-equal?
        (new-move (new-position) 'white "e4")
        (location (new-location "e2") (new-location "e4"))))

    (test-case "char-diff"
      (check-equal? 0 (char-diff #\a #\a))
      (check-equal? 7 (char-diff #\8 #\1)))

    (test-case "all-locations"
      (check-not-false
        (for*/fold ([accum #t]) ([file (in-range 8)] [rank (in-range 8)])
          (and (member (location file rank) all-locations) accum)))
      (check-equal? (length all-locations) 64))

    (test-case "valid-move"
      (define (parse-move a b) (move (new-move (new-position) 'white a)
                                     (new-move (new-position) 'white b)))
      (check-true (valid-move (new-position) (parse-move "c2" "c3")) "c3")
      (check-true (valid-move (new-position) (parse-move "b1" "c3")) "Nc3"))
  ))

(require rackunit/text-ui)
(run-tests chess-tests)

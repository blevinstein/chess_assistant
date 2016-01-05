#lang racket

(require rackunit)
(require "chess.rkt")

(define (parse-move a b) (list (move (new-location a) (new-location b))))

(define chess-tests
  (test-suite "Chess Tests"

    (test-case "new-location"
      (check-equal? (new-location "a1") (location 0 0))
      (check-equal? (new-location "c4") (location 2 3))
      (check-equal? (new-location "h8") (location 7 7)))

    (test-case "new-grid"
      ; check length and width
      (check-equal? (length (new-grid)) 8)
      (check-equal? (length (list-ref (new-grid) 0)) 8))

    (test-case "grid-ref"
      (check-equal? (grid-ref (new-grid) (location 0 0)) (cons 'white 'R))
      (check-equal? (grid-ref (new-grid) (location 0 1)) (cons 'white 'P))
      (check-equal? (grid-ref (new-grid) (location 1 0)) (cons 'white 'N)))

    (test-case "new-position"
      ; check length
      (check-equal? (length (new-position)) 32))

    (test-case "new-move"
      (check-equal?
        (new-move (new-position) 'white "e4")
        (list (move (new-location "e2") (new-location "e4")))))

    (test-case "char-diff"
      (check-equal? 0 (char-diff #\a #\a))
      (check-equal? 7 (char-diff #\8 #\1)))

    (test-case "all-locations"
      (check-not-false
        (for*/fold ([accum #t]) ([file (in-range 8)] [rank (in-range 8)])
          (and (member (location file rank) all-locations) accum)))
      (check-equal? (length all-locations) 64))

    (test-case "valid-move"
      (check-true (valid-move (new-position) (parse-move "c2" "c3")) "c3")
      (check-true (valid-move (new-position) (parse-move "b1" "c3")) "Nc3")
      (check-false (valid-move (new-position) (parse-move "c2" "c5")) "!"))

    (test-case "pawn-moves"
      (check-equal?
        (list (parse-move "c2" "c3") (parse-move "c2" "c4"))
        (pawn-moves (new-position) (new-location "c2"))))

    (test-case "knight-moves"
      (check-equal?
        (set (parse-move "b1" "a3") (parse-move "b1" "c3") (parse-move "b1" "d2"))
        (list->set (knight-moves (new-position) (new-location "b1")))))

    (test-case "rook-moves"
      (check-equal?
        (set (parse-move "a1" "a2") (parse-move "a1" "b1"))
        (list->set (rook-moves (new-position) (new-location "a1"))))
      (check-equal?
        (list->set (rook-moves (new-position) (new-location "a3")))
        (set (parse-move "a3" "b3") (parse-move "a3" "c3") (parse-move "a3" "d3")
             (parse-move "a3" "e3") (parse-move "a3" "f3") (parse-move "a3" "g3")
             (parse-move "a3" "h3")
             (parse-move "a3" "a2")
             (parse-move "a3" "a4") (parse-move "a3" "a5") (parse-move "a3" "a6")
             (parse-move "a3" "a7"))))

    (test-case "queen-moves"
      (check-equal?
        (list->set (queen-moves (new-position) (new-location "d5")))
        (set
          (parse-move "d5" "c6") (parse-move "d5" "b7")
          (parse-move "d5" "d6") (parse-move "d5" "d7")
          (parse-move "d5" "e6") (parse-move "d5" "f7")
          (parse-move "d5" "c4") (parse-move "d5" "b3") (parse-move "d5" "a2")
          (parse-move "d5" "e4") (parse-move "d5" "f3") (parse-move "d5" "g2")
          (parse-move "d5" "d4") (parse-move "d5" "d3") (parse-move "d5" "d2")
          (parse-move "d5" "c5") (parse-move "d5" "b5") (parse-move "d5" "a5")
          (parse-move "d5" "e5") (parse-move "d5" "f5") (parse-move "d5" "g5")
          (parse-move "d5" "h5"))))

    (test-case "defenders"
      (check-equal?
        (defenders (new-position) (new-location "f2"))
        (list (new-location "e1"))))
  ))

(require rackunit/text-ui)
(run-tests chess-tests)

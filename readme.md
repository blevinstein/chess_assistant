README
======

Simple chess assistant. Helps the player visualize the board.

To run on command line:

> sbt repl

To run in browser:

> sbt server

To run tests:

> sbt test

TODO
====

- Reimplement threat calculation.
  - Difficult because defending moves arent legal.
- Reimplement server functionality.

Projects:

TODO: Audit existing polymer code
TODO: Design check avoidance (hard)
  - King cannot move into check
  - King cannot pass through check while castling
  - A piece that has been pinned to the King can only move along the pin
TODO: Research algorithms for coloring the board
TODO: def Move.chessNotation: String, generate compact but unambiguous notation

OLD Readme
----------

OLD implementation in Racket. NEW implementation in Scala.

To run on command line:

> racket src/racket/repl.rkt

To run in browser:

> ./serve-racket.sh
(then navigate to http://localhost:8000)

To run tests:

> racket src/racket/chess-test.rkt

TODO
----

- servlet
  - add ability to rotate board
  - persist history

- refactor json conversion code

- rider-shadow
  - pin
  - skewer
  - lance
  - hidden attack
- attacked by lower value piece

- pawn promotion
- history (list of positions)
  - castling
  - en passant
- check, checkmate, draw

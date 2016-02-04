README
======

Simple chess assistant. Helps the player visualize the board.

To run on command line:

> racket src/repl.rkt

To run in browser:

> ./serve.sh
(then navigate to http://localhost:8000)

To run tests:

> racket src/chess-test.rkt

TODO
====

Scala Rewrite
-------------

- Reimplement basic chess rules.
- Reimplement basic REPL.
- Reimplement server functionality.

Projects:

TODO: Audit existing polymer code
TODO: Design castling (easyish)
TODO: Design en passant (medium)
TODO: Design check avoidance (hard)
  - King cannot move into check
  - King cannot pass through check while castling
  - A piece that has been pinned to the King can only move along the pin
TODO: Research algorithms for coloring the board

Old TODO List
-------------

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

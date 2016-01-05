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

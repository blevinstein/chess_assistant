README
======

Simple chess assistant. Helps the player visualize the board.

Install dependencies:

> sbt update

To run on command line:

> sbt repl

To run in browser:

> sbt server

To run tests:

> sbt test

TODO
----

- Add special treatment of King
  - Position#inCheck(color: Color = toMove)
  - CantMoveIntoCheck(threats: List[Location]) extends InvalidReason
  - Cant move through check while castling
- implicit def createLocation(s: String): Location
- Reimplement server in React.
- Research algorithms for coloring the board
- def Move.chessNotation: String, generate compact but unambiguous notation

OLD Readme
==========

OLD implementation in Racket. NEW implementation in Scala.

Install dependencies (requires bower):

> bower update

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

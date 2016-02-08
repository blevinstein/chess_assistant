package com.blevinstein.chess

import org.scalatest._

class PositionTest extends FunSuite with Matchers {
  test("initial") {
    Position.initial(Location("a1")) shouldEqual Some(White, Rook)
    Position.initial(Location("a3")) shouldEqual None
    Position.initial(Location("d1")) shouldEqual Some(White, Queen)
    Position.initial(Location("e1")) shouldEqual Some(White, King)

    Position.initial(Location("d8")) shouldEqual Some(Black, Queen)
    Position.initial(Location("e8")) shouldEqual Some(Black, King)
  }

  test("update") {
    val pos = Position.initial.update(Map(
        Location("a2") -> None,
        Location("a3") -> Some(White, Pawn)))

    pos(Location("a2")) shouldEqual None
    pos(Location("a3")) shouldEqual Some(White, Pawn)
    pos.toMove shouldEqual Black
    pos.history shouldEqual List(Position.initial)
  }

  test("rewind") {
    val pos = Position.initial.
        update(Map(
            Location("a2") -> None, Location("a3") -> Some(White, Pawn))).
        rewind

    pos shouldEqual Position.initial
  }

  test("getAllMoves - B on b2") {
    val pos = Position(
        Map(Location("b2") -> Some(White, Bishop)).withDefaultValue(None),
        White,
        List.empty)
    Set(pos.getAllMoves:_*) shouldEqual
        Set(RiderMove(Location("b2"), (1, 1), 1), // c3
            RiderMove(Location("b2"), (1, 1), 2), // d4
            RiderMove(Location("b2"), (1, 1), 3), // e5
            RiderMove(Location("b2"), (1, 1), 4), // f6
            RiderMove(Location("b2"), (1, 1), 5), // g7
            RiderMove(Location("b2"), (1, 1), 6), // h8
            RiderMove(Location("b2"), (-1, -1), 1), // a1
            RiderMove(Location("b2"), (-1, 1), 1), // a3
            RiderMove(Location("b2"), (1, -1), 1)) // c1
  }

  test("getAllMoves - N on d5") {
    val pos = Position(
        Map(Location("d5") -> Some(Black, Knight)).withDefaultValue(None),
        Black,
        List.empty)
    Set(pos.getAllMoves:_*) shouldEqual
        Set(LeaperMove(Location("d5"), (1, 2)), // e7
            LeaperMove(Location("d5"), (2, 1)), // f6
            LeaperMove(Location("d5"), (2, -1)), // f4
            LeaperMove(Location("d5"), (1, -2)), // e3
            LeaperMove(Location("d5"), (-1, 2)), // c7
            LeaperMove(Location("d5"), (-2, 1)), // b6
            LeaperMove(Location("d5"), (-2, -1)), // b4
            LeaperMove(Location("d5"), (-1, -2))) // c3
  }

  test("getAllMoves - R and K at initial positions") {
    val pos = Position(
        Map(Location("a1") -> Some(White, Rook),
            Location("e1") -> Some(White, King),
            Location("h1") -> Some(White, Rook),
            Location("a8") -> Some(Black, Rook),
            Location("e8") -> Some(Black, King),
            Location("h8") -> Some(Black, Rook)).withDefaultValue(None),
        White,
        List.empty)
    pos.getAllMoves.contains(Castle(White, true)) shouldEqual true
    pos.getAllMoves.contains(Castle(White, false)) shouldEqual true
    pos.getAllMoves.contains(Castle(Black, true)) shouldEqual true
    pos.getAllMoves.contains(Castle(Black, false)) shouldEqual true
  }
}

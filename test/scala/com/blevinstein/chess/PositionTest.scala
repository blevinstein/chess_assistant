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
}

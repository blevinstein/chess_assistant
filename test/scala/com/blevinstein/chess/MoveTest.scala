package com.blevinstein.chess

import org.scalatest._

class MoveTest extends FunSuite with Matchers {
  test("firstMove") {
    val pos = Position.initial.
        update(Map(Location("a1") -> None, Location("a4") -> Some(White, Rook)))

    Move.firstMove(pos, Location("a1")) shouldEqual false
    Move.firstMove(pos, Location("h1")) shouldEqual true

    Move.firstMove(pos, Location("a8")) shouldEqual true
    Move.firstMove(pos, Location("h8")) shouldEqual true
  }

  test("allTransformations") {
    Set(Move.allTransformations((2, 2)):_*) shouldEqual
        Set((2, 2), (-2, 2), (2, -2), (-2, -2))
    Set(Move.allTransformations((1, 3)):_*) shouldEqual Set(
        (1, 3), (-1, 3), (1, -3), (-1, -3), (3, 1), (-3, 1), (3, -1), (-3, -1))
  }

  test("LeaperMove") {
    val pos = LeaperMove(Location("b1"), (1, 2))(Position.initial).get
    pos(Location("b1")) shouldEqual None
    pos(Location("c3")) shouldEqual Some(White, Knight)

    LeaperMove(Location("b8"), (1, -2)).isLegal(pos) shouldEqual true
    LeaperMove(Location("b8"), (2, -1)).isLegal(pos) shouldEqual false
  }

  test("RiderMove") {
    val pos = RiderMove(Location("b2"), (1, 1), 3)(Position.initial).get
    pos(Location("b2")) shouldEqual None
    pos(Location("e5")) shouldEqual Some(White, Pawn)

    RiderMove(Location("e5"), (1, 1), 1).isLegal(pos) shouldEqual true
    RiderMove(Location("e5"), (1, 1), 2).isLegal(pos) shouldEqual true
    RiderMove(Location("e5"), (1, 1), 3).isLegal(pos) shouldEqual false
  }
}

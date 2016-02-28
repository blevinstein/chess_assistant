package com.blevinstein.chess

import com.blevinstein.chess.LocationImplicits._

import org.scalatest._

class PositionTest extends FunSuite with Matchers {
  test("initial") {
    Position.initial("a1") shouldEqual Some(White, Rook)
    Position.initial("a3") shouldEqual None
    Position.initial("d1") shouldEqual Some(White, Queen)
    Position.initial("e1") shouldEqual Some(White, King)

    Position.initial("d8") shouldEqual Some(Black, Queen)
    Position.initial("e8") shouldEqual Some(Black, King)
  }

  test("update") {
    val pos = Position.initial.update(Map(
        Location("a2") -> None,
        Location("a3") -> Some(White, Pawn)))

    pos("a2") shouldEqual None
    pos("a3") shouldEqual Some(White, Pawn)
    pos.toMove shouldEqual Black
    pos.history shouldEqual List(Position.initial)
  }

  test("rewind") {
    val pos = Position.initial.
        update(
            Map(Location("a2") -> None, Location("a3") -> Some(White, Pawn))).
        rewind

    pos shouldEqual Position.initial
  }

  test("getAllMoves - B on b2") {
    val pos = Position(
        Map(Location("b2") -> Some(White, Bishop)).withDefaultValue(None),
        White,
        List.empty)
    Set(pos.getAllMoves:_*) shouldEqual
        Set(RiderMove("b2", (1, 1), 1), // c3
            RiderMove("b2", (1, 1), 2), // d4
            RiderMove("b2", (1, 1), 3), // e5
            RiderMove("b2", (1, 1), 4), // f6
            RiderMove("b2", (1, 1), 5), // g7
            RiderMove("b2", (1, 1), 6), // h8
            RiderMove("b2", (-1, -1), 1), // a1
            RiderMove("b2", (-1, 1), 1), // a3
            RiderMove("b2", (1, -1), 1)) // c1
  }

  test("getAllMoves - N on d5") {
    val pos = Position(
        Map(Location("d5") -> Some(Black, Knight)).withDefaultValue(None),
        Black,
        List.empty)
    Set(pos.getAllMoves:_*) shouldEqual
        Set(LeaperMove("d5", (1, 2)), // e7
            LeaperMove("d5", (2, 1)), // f6
            LeaperMove("d5", (2, -1)), // f4
            LeaperMove("d5", (1, -2)), // e3
            LeaperMove("d5", (-1, 2)), // c7
            LeaperMove("d5", (-2, 1)), // b6
            LeaperMove("d5", (-2, -1)), // b4
            LeaperMove("d5", (-1, -2))) // c3
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

  test("isAttacking") {
    val pos = Position(
        Map(Location("d1") -> Some(White, Queen),
            Location("c2") -> Some(White, Pawn),
            Location("d3") -> Some(White, Pawn),
            Location("a4") -> Some(White, Pawn),
            Location("d4") -> Some(Black, Queen)).withDefaultValue(None),
        White,
        List.empty)

    // White queen
    pos.isAttacking("d1", "c2") shouldEqual false
    pos.isAttacking("d1", "d3") shouldEqual false
    pos.isAttacking("d1", "a4") shouldEqual false
    pos.isAttacking("d1", "d4") shouldEqual false
    pos.isAttacking("d1", "g4") shouldEqual true

    // Black queen
    pos.isAttacking("d4", "d1") shouldEqual false
    pos.isAttacking("d4", "d3") shouldEqual true
    pos.isAttacking("d4", "a4") shouldEqual true
    pos.isAttacking("d4", "c2") shouldEqual false
    pos.isAttacking("d4", "g4") shouldEqual true
  }

  test("isDefending") {
    val pos = Position(
        Map(Location("d1") -> Some(White, Queen),
            Location("c2") -> Some(White, Pawn),
            Location("d3") -> Some(White, Pawn),
            Location("a4") -> Some(White, Pawn),
            Location("d4") -> Some(Black, Queen)).withDefaultValue(None),
        White,
        List.empty)

    pos.isDefending("d1", "c2") shouldEqual true
    pos.isDefending("d1", "d3") shouldEqual true
    pos.isDefending("d1", "a4") shouldEqual false
    pos.isDefending("d1", "d4") shouldEqual false
    pos.isDefending("d1", "g4") shouldEqual true

    // Black queen
    pos.isDefending("d4", "d1") shouldEqual false
    pos.isDefending("d4", "d3") shouldEqual false
    pos.isDefending("d4", "a4") shouldEqual false
    pos.isDefending("d4", "c2") shouldEqual false
    pos.isDefending("d4", "g4") shouldEqual true
  }
}

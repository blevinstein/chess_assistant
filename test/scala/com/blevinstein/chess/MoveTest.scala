package com.blevinstein.chess

import com.blevinstein.chess.LocationImplicits._

import org.scalatest._

class MoveTest extends FunSuite with Matchers {
  // Helper function, makes it easier to compare positions for equality.
  // TODO: fix Position.equals (and hashcode; difficult)
  def cleanup(pos: Position): Position = Position(
      pos.map.toList.filter{ case (k, v) => v != None }.toMap,
      pos.toMove,
      pos.history)

  test("firstMove") {
    val pos = Position.initial.
        update(Map(Location("a1") -> None, Location("a4") -> Some(White, Rook)))

    Move.firstMove(pos, "a1") shouldEqual false
    Move.firstMove(pos, "h1") shouldEqual true

    Move.firstMove(pos, "a8") shouldEqual true
    Move.firstMove(pos, "h8") shouldEqual true
  }

  test("allTransformations") {
    Set(Move.allTransformations((2, 2)):_*) shouldEqual
        Set((2, 2), (-2, 2), (2, -2), (-2, -2))
    Set(Move.allTransformations((1, 3)):_*) shouldEqual Set(
        (1, 3), (-1, 3), (1, -3), (-1, -3), (3, 1), (-3, 1), (3, -1), (-3, -1))
  }

  test("between") {
    Move.between(0, 5).toList shouldEqual List(1, 2, 3, 4)
    Move.between(5, 0).toList shouldEqual List(1, 2, 3, 4)

    Move.between(0, 0).toList shouldEqual List()
    Move.between(0, 1).toList shouldEqual List()
    Move.between(1, 0).toList shouldEqual List()
  }

  test("LeaperMove") {
    val pos = LeaperMove("b1", (1, 2))(Position.initial).right.get
    pos("b1") shouldEqual None
    pos("c3") shouldEqual Some(White, Knight)

    LeaperMove("b8", (1, -2)).isLegal(pos) shouldEqual true
    LeaperMove("b8", (2, -1)).isLegal(pos) shouldEqual false
  }

  test("RiderMove") {
    val pos = Position.create(List("d4", "d5", "Bf4", "a5"))
    pos("c1") shouldEqual None
    pos("f4") shouldEqual Some(White, Bishop)

    RiderMove("f4", (1, 1), 1).isLegal(pos) shouldEqual true
    RiderMove("f4", (1, 1), 2).isLegal(pos) shouldEqual true
    RiderMove("f4", (-1, 1), 1).isLegal(pos) shouldEqual true
    RiderMove("f4", (-1, 1), 2).isLegal(pos) shouldEqual true
    RiderMove("f4", (-1, 1), 3).isLegal(pos) shouldEqual true
    RiderMove("f4", (-1, 1), 4).isLegal(pos) shouldEqual false
  }

  test("Castle") {
    val initial = Position(Map(
            Location("a1") -> Some(White, Rook),
            Location("e1") -> Some(White, King))
            .withDefaultValue(None),
        White,
        List())

    val expected = Position(Map(
            Location("d1") -> Some(White, Rook),
            Location("c1") -> Some(White, King))
            .withDefaultValue(None),
        Black,
        List(initial))

    val whiteQueenside = Castle(White, kingside = false)
    cleanup(whiteQueenside(initial).right.get) shouldEqual expected
  }

  test("Move.create - initial position") {
    val pos = Position.initial
    Move.create(pos, "a3") shouldEqual
        CustomMove("a2", "a3", canCapture = false)
    Move.create(pos, "a4") shouldEqual
        CustomMove("a2", "a4", canCapture = false)
    Move.create(pos, "Nc3") shouldEqual LeaperMove("b1", (1, 2))
  }

  test("Move.create - e4 opening") {
    val pos1 = Move.create(Position.initial, "e4")(Position.initial).right.get
    val pos2 = Move.create(pos1, "e5")(pos1).right.get
    Move.create(pos2, "Qg4") shouldEqual RiderMove("d1", (1, 1), 3)
    Move.create(pos2, "Ba6") shouldEqual RiderMove("f1", (-1, 1), 5)
    Move.create(pos2, "Ne2") shouldEqual LeaperMove("g1", (-2, 1))
  }

  test("Move.create - e4 Nf3 Bd3 opening") {
    val pos = Position.create(List("e4", "e5", "Nf3", "Nf6", "Bd3", "Bd6"))
    Move.create(pos, "O-O") shouldEqual Castle(White, kingside = true)
    Move.create(pos, "Rf1") shouldEqual RiderMove("h1", (-1, 0), 2)
  }

  test("createSourcePredicate") {
    Move.createSourcePredicate("a")(Location("a3")) shouldEqual true
    Move.createSourcePredicate("a")(Location("b3")) shouldEqual false

    Move.createSourcePredicate("4")(Location("c4")) shouldEqual true
    Move.createSourcePredicate("4")(Location("c7")) shouldEqual false

    Move.createSourcePredicate("a4")(Location("a4")) shouldEqual true
    Move.createSourcePredicate("a4")(Location("b4")) shouldEqual false
    Move.createSourcePredicate("a4")(Location("a3")) shouldEqual false
  }

  test("http://www.chessgames.com/perl/chessgame?gid=1106430") {
    val pos = Position.create(List(
        "e4", "c6", // 1
        "Nc3", "d5",
        "Nf3", "Bg4",
        "h3", "Bf3",
        "Qf3", "Nf6",
        "d3", "e6", // 6
        "g3", "Bb4",
        "Bd2", "d4",
        "Nb1", "Bd2",
        "Nd2", "e5",
        "Bg2", "c5", // 11
        "O-O", "Nc6",
        "Qe2", "Qe7",
        "f4", "O-O-O",
        "a3", "Ne8",
        "b4", "cb4", // 16
        "Nc4", "f6",
        "fe5", "fe5",
        "ab4", "Nc7",
        "Na5", "Nb5",
        "Nc6", "bc6", // 21
        "Rf2", "g6",
        "h4", "Kb7",
        "h5", "Qb4",
        "Rf7", "Kb6",
        "Qf2", "a5", // 26
        "c4", "Nc3",
        "Rf1", "a4",
        "Qf6", "Qc5",
        "Rh7", "Rdf8",
        "Qg6", "Rh7", // 31
        "Qh7", "Rf1",
        "Bf1", "a3",
        "h6", "a2",
        "Qg8", "a1Q",
        "h7", "Qd6", // 36
        "h8Q", "Qa7",
        "g4", "Kc5",
        "Qf8", "Qae7",
        "Qa8", "Kb4",
        "Qh2", "Kb3", // 41
        "Qa1", "Qa3",
        "Qa3", "Ka3",
        "Qh6", "Qf7",
        "Kg2", "Kb3",
        "Qd2", "Qh7", // 46
        "Kg3", "Qe4",
        "Qf2", "Qh1"))

    cleanup(pos).map shouldEqual Map(
        Location("b3") -> Some(Black, King),
        Location("c3") -> Some(Black, Knight),
        Location("h1") -> Some(Black, Queen),
        Location("d4") -> Some(Black, Pawn),
        Location("e5") -> Some(Black, Pawn),
        Location("c6") -> Some(Black, Pawn),
        Location("c4") -> Some(White, Pawn),
        Location("d3") -> Some(White, Pawn),
        Location("f1") -> Some(White, Bishop),
        Location("f2") -> Some(White, Queen),
        Location("g3") -> Some(White, King),
        Location("g4") -> Some(White, Pawn))
    pos.toMove shouldEqual White
  }

  test("http://www.chessgames.com/perl/chessgame?gid=1008419") {
    val pos = Position.create(List(
        "d4", "Nf6",
        "c4", "g6",
        "g3", "c6",
        "Bg2", "d5",
        "cd5", "cd5",
        "Nc3", "Bg7",
        "e3", "O-O",
        "Nge2", "Nc6",
        "O-O", "b6",
        "b3", "Ba6",
        "Ba3", "Re8",
        "Qd2", "e5",
        "de5", "Ne5",
        "Rfd1", "Nd3",
        "Qc2", "Nf2",
        "Kf2", "Ng4",
        "Kg1", "Ne3",
        "Qd2", "Ng2",
        "Kg2", "d4",
        "Nd4", "Bb7",
        "Kf1", "Qd7"))

    cleanup(pos).map shouldEqual Map(
        Location("a8") -> Some(Black, Rook),
        Location("a7") -> Some(Black, Pawn),
        Location("b7") -> Some(Black, Bishop),
        Location("b6") -> Some(Black, Pawn),
        Location("d7") -> Some(Black, Queen),
        Location("e8") -> Some(Black, Rook),
        Location("f7") -> Some(Black, Pawn),
        Location("g6") -> Some(Black, Pawn),
        Location("g7") -> Some(Black, Bishop),
        Location("g8") -> Some(Black, King),
        Location("h7") -> Some(Black, Pawn),

        Location("a1") -> Some(White, Rook),
        Location("a2") -> Some(White, Pawn),
        Location("a3") -> Some(White, Bishop),
        Location("b3") -> Some(White, Pawn),
        Location("c3") -> Some(White, Knight),
        Location("d4") -> Some(White, Knight),
        Location("d2") -> Some(White, Queen),
        Location("d1") -> Some(White, Rook),
        Location("f1") -> Some(White, King),
        Location("g3") -> Some(White, Pawn),
        Location("h2") -> Some(White, Pawn))
  }

  test("http://www.chessgames.com/perl/chessgame?gid=1410669 en passant") {
    val pos = Position.create(List(
        "e4", "c5",
        "Nf3", "e6",
        "d4", "cd4",
        "Nd4", "a6",
        "Nc3", "Qc7", // 5
        "Bd3", "Nf6",
        "O-O", "Bc5",
        "Nb3", "Be7",
        "f4", "d6",
        "a4", "Nc6", // 10
        "a5", "b5",
        "ab6", "Qb6",
        "Kh1", "O-O",
        "Qe2", "a5",
        "Be3", "Qc7", // 15
        "Nb5", "Qb8",
        "c3", "d5",
        "e5", "Ne4",
        "Be4", "de4",
        "Nc5", "Bc5", // 20
        "Bc5", "Ba6",
        "c4", "Rd8",
        "Nd6", "f5",
        "ef6", "Rd6",
        "Qe4", "Bb7", // 25
        "Bd6", "Qd6",
        "Rad1", "Nd8",
        "f7", "Kf7",
        "Qh7", "Qc6",
        "Rf2", "Qe4", // 30
        "f5", "e5",
        "Rfd2", "Bc6",
        "Qg6", "Ke7",
        "Rd7"))

    cleanup(pos).map shouldEqual Map(
        Location("a8") -> Some(Black, Rook),
        Location("a5") -> Some(Black, Pawn),
        Location("c6") -> Some(Black, Bishop),
        Location("d8") -> Some(Black, Knight),
        Location("e7") -> Some(Black, King),
        Location("e5") -> Some(Black, Pawn),
        Location("e4") -> Some(Black, Queen),
        Location("g7") -> Some(Black, Pawn),
        Location("b2") -> Some(White, Pawn),
        Location("c4") -> Some(White, Pawn),
        Location("d1") -> Some(White, Rook),
        Location("d7") -> Some(White, Rook),
        Location("f5") -> Some(White, Pawn),
        Location("g6") -> Some(White, Queen),
        Location("g2") -> Some(White, Pawn),
        Location("h2") -> Some(White, Pawn),
        Location("h1") -> Some(White, King))
  }
}

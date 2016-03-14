package com.blevinstein.chess

trait Piece {
  def letter: String
  def getMoves(position: Position, location: Location): List[Move]

  override def toString: String = this.letter
}
object Piece {
  val byLetter: Map[String, Piece] =
      List(Pawn, Knight, Bishop, Rook, Queen, King).
      map(piece => piece.letter -> piece).
      toMap
}

object Pawn extends Piece {
  def letter: String = "P"
  def getMoves(position: Position, location: Location): List[Move] = {
    val pieceColor = position(location).get._1

    val forwardDir = if (pieceColor == White) (0, 1) else (0, -1)

    val forward = location + forwardDir
    val doubleForward = location + forwardDir + forwardDir
    val left = location + forwardDir + (-1, 0)
    val right = location + forwardDir + (1, 0)

    val enPassantPossible = ((pieceColor == White && location.rank == 4) ||
            (pieceColor == Black && location.rank == 3))

    List(
        Some(CustomMove(location, forward, canCapture = false)),
        if (left.isValid) Some(CustomMove(location, left, mustCapture = true))
        else None,
        if (left.isValid && enPassantPossible) Some(EnPassant(location, left))
        else None,
        if (right.isValid) Some(CustomMove(location, right, mustCapture = true))
        else None,
        if (right.isValid && enPassantPossible) Some(EnPassant(location, right))
        else None,
        if (Move.firstMove(position, location))
            Some(CustomMove(location, doubleForward, canCapture = false))
        else None).flatten[Move]
  }
}

object Knight extends Piece {
  def letter: String = "N"
  def getMoves(position: Position, location: Location): List[Move] =
      LeaperMove.all(location, (1, 2))
}

object Bishop extends Piece {
  def letter: String = "B"
  def getMoves(position: Position, location: Location): List[Move] =
      RiderMove.all(location, (1, 1))
}

object Rook extends Piece {
  def letter: String = "R"
  def getMoves(position: Position, location: Location): List[Move] =
      RiderMove.all(location, (0, 1))
}

object Queen extends Piece {
  def letter: String = "Q"
  def getMoves(position: Position, location: Location): List[Move] =
      (RiderMove.all(location, (0, 1)) ++ RiderMove.all(location, (1, 1)))
}

object King extends Piece {
  def letter: String = "K"
  def getMoves(position: Position, location: Location): List[Move] =
      LeaperMove.all(location, (1, 1)) ++
      LeaperMove.all(location, (0, 1)) ++
      (if (Move.firstMove(position, location)) Castle.getAll else List.empty)
}

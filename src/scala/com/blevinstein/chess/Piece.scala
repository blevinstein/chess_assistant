package com.blevinstein.chess

trait Piece {
  def letter: String
  def getMoves(position: Position, location: Location): List[Move]

  override def toString: String = this.getClass.getSimpleName
}
object Piece {
  val byLetter: Map[String, Piece] = List(Pawn, Knight, Bishop, Rook, Queen, King).
      map(piece => piece.letter -> piece)
      .toMap
}

object Pawn extends Piece {
  def letter: String = "P"
  def getMoves(position: Position, location: Location): List[Move] = {
    val pieceColor = position(location).get._1

    val forward = if (pieceColor == White) (0, 1) else (0, -1)
    val left = (-1, 0)
    val right = (1, 0)

    // TODO: add en passant
    (List(CustomMove(location, location + forward, canCapture = false),
            CustomMove(location, location + forward + left, mustCapture = true),
            CustomMove(location, location + forward + right, mustCapture = true))
            ++
            (if (Move.firstMove(position, location))
                List(CustomMove(location,
                    location + forward + forward,
                    canCapture = false))
            else List.empty))
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
      (LeaperMove.all(location, (1, 1)) ++ LeaperMove.all(location, (0, 1)))
}

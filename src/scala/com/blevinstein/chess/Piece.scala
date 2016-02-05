package com.blevinstein.chess

trait Piece {
  def letter: String
  def getMoves(position: Position, location: Location): List[Move]
}

object Pawn extends Piece {
  def letter: String = "P"
  def getMoves(position: Position, location: Location): List[Move] = {
    val pieceColor = position(location).get._1

    val forward = if (pieceColor == White) (0, 1) else (0, -1)
    val left = (-1, 0)
    val right = (1, 0)

    val firstMove = Move.firstMove(position, location)

    // TODO: incomplete
    List(CustomMove(location, location + forward, canCapture = false)).
        filter{_.isLegal(position)}
  }
}

object Knight extends Piece {
  def letter: String = "N"
  def getMoves(position: Position, location: Location):
      List[Move] = LeaperMove.all(location, (1, 2)).filter{_.isLegal(position)}
}

object Bishop extends Piece {
  def letter: String = "B"
  def getMoves(position: Position, location: Location):
      List[Move] = RiderMove.all(location, (1, 1)).filter{_.isLegal(position)}
}

object Rook extends Piece {
  def letter: String = "R"
  def getMoves(position: Position, location: Location):
      List[Move] = RiderMove.all(location, (0, 1)).filter{_.isLegal(position)}
}

object Queen extends Piece {
  def letter: String = "Q"
  def getMoves(position: Position, location: Location):
      List[Move] =
          (RiderMove.all(location, (0, 1)) ++ RiderMove.all(location, (1, 1))).
          filter{_.isLegal(position)}
}

object King extends Piece {
  def letter: String = "K"
  def getMoves(position: Position, location: Location):
      List[Move] = LeaperMove.all(location, (1, 1)).filter{_.isLegal(position)}
}

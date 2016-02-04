package com.blevinstein.chess

trait Move {
  // NOTE: To fully embed information about valid moves, we need to have access
  // to the entire history of the board. Consider en passant and castling.
  def apply(history: History, position: Position):
      Option[Position]
}

object Move {
  // Create a move, given [input] in chess notation
  def infer(position: Position, input: String) = input match {
    case _ => ???
  }

  def infer(
      position: Position,
      hintPredicate: Location => Boolean = (_) => true,
      promote: Option[Piece] = None) {
    ???
  }
  // Helper methods

  def allTransformations(offset: (Int, Int)): List[(Int, Int)] = offset match {
    case (f, r) => Set(
        (f, r), (-f, r), (f, -r), (-f, -r),
        (r, f), (-r, f), (r, -f), (-r, -f)).toList
  }

  def mul(offset: (Int, Int), k: Int): (Int, Int) =
      (offset._1 * k, offset._2 * k)

  def tryMove(
      position: Position,
      source: Location,
      dest: Location): Option[Position] = {
    (position(source), position(dest)) match {
      // Move to open space
      case (Some((sourceColor, piece)), None)
          if sourceColor == position.toMove =>
          Some((position +
              (source, None) +
              (dest, Some((sourceColor, piece)))).nextMove)
      // Capture
      case (Some((sourceColor, piece)), Some((destColor, _)))
          if sourceColor == position.toMove && destColor == !position.toMove =>
          Some((position +
              (source, None) +
              (dest, Some((position.toMove, piece)))).nextMove)
      // Can't move
      case _ => None
    }
  }
}

object LeaperMove {
  def all(source: Location, offset: (Int, Int)): List[LeaperMove] =
      Move.allTransformations(offset).
          filter(offset => (source + offset).isValid). // bounds check
          map(offset => LeaperMove(source, offset)).
          toList
}
case class LeaperMove(source: Location, offset: (Int, Int)) extends Move {
  def apply(history: History, position: Position):
      Option[Position] =
      Move.tryMove(position, source, source + offset)
}

object RiderMove {
  def all(source: Location, offset: (Int, Int)): List[RiderMove] =
      Move.allTransformations(offset).
          map(offset =>
              (1 to 8).map(dist => (offset, dist))
          ).flatten.
          filter{ case (offset, dist) =>
              (source + Move.mul(offset, dist)).isValid
          }.
          map{ case (offset, dist) => RiderMove(source, offset, dist) }.
          toList
}
case class RiderMove(source: Location, offset: (Int, Int), dist: Int) extends
    Move {

  def apply(history: History, position: Position):
      Option[Position] = {
    val emptyBetween = (1 until dist).
        forall(i => position(source + Move.mul(offset, i + 1)) == None)

    if (emptyBetween) {
      Move.tryMove(position, source, source + Move.mul(offset, dist))
    } else {
      None
    }
  }
}


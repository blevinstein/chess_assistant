package com.blevinstein.chess

// There are two types of moves.
// 1. Piece moves. Each living Pawn/Knight/Bishop/Rook/Queen/King may have
//    available moves.
// 2. Castling. Moves two pieces at once, is only allowed under specific
//    conditions. (Any other exceptions?)

trait Move {
  // Returns the new state of the board after making [this] move.
  // Should return None if [this] is not a valid move.
  def apply(history: History, position: Position): Option[Position]
  // Returns true if [this] is is a legal move that could be made when the board
  // is in [position], after [history].
  // NOTE: To fully embed information about legal moves, we need to have access
  // to the entire history of the board. Consider en passant and castling.
  def isLegal(history: History, position: Position): Boolean =
      apply(history, position) != None
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

  // Attempt to move a piece from [source] to [destination].
  // Does not check for occluding pieces in between.
  def tryMove(
      position: Position,
      source: Location,
      dest: Location,
      canCapture: Boolean = true,
      mustCapture: Boolean = false):
      Option[Position] = {
    (position(source), position(dest)) match {
      // Move to open space
      case (Some((sourceColor, piece)), None)
          if sourceColor == position.toMove && !mustCapture =>
          Some(position.update(Map(
              source -> None,
              dest -> Some((sourceColor, piece)))))
      // Capture
      case (Some((sourceColor, piece)), Some((destColor, _)))
          if sourceColor == position.toMove &&
              destColor == !position.toMove &&
              canCapture =>
          Some(position.update(Map(
              source -> None,
              dest -> Some((sourceColor, piece)))))
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


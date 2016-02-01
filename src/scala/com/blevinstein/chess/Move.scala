package com.blevinstein.chess

object Move {
  def allTransformations(offset: (Int, Int)): List[(Int, Int)] = offset match {
    case (f, r) => Set(
        (f, r), (-f, r), (f, -r), (-f, -r),
        (r, f), (-r, f), (r, -f), (-r, -f)).toList
  }
  def tryMove(
      position: Position,
      toMove: Color,
      source: Location,
      dest: Location): Option[Position] = {
    (position(source), position(dest)) match {
      case (Some((sourceColor, piece)), None)
          if sourceColor == toMove =>
          Some(position + (source, None) + (dest, Some((sourceColor, piece))))
      case (Some((sourceColor, piece)), Some((destColor, _)))
          if sourceColor == toMove && destColor == !toMove =>
          // Capture
          Some(position + (source, None) + (dest, Some((toMove, piece))))
      case _ => None
    }
  }
}

trait Move {
  // NOTE: To fully embed information about valid moves, we need to have access
  // to the entire history of the board. Consider en passant and castling.
  def apply(history: History, position: Position, toMove: Color):
      Option[Position]
}

object LeaperMove {
  def all(source: Location, offset: (Int, Int)) =
      Move.allTransformations(offset).
          filter(o => (source + o).isValid). // bounds check
          map{LeaperMove(source, _)}.
          toList
}
case class LeaperMove(source: Location, offset: (Int, Int)) extends Move {
  def apply(history: History, position: Position, toMove: Color):
      Option[Position] =
      Move.tryMove(position, toMove, source, source + offset)
}

case class RiderMove(source: Location, offset: (Int, Int), dist: Int) extends
    Move {
  private def mul(offset: (Int, Int), k: Int): (Int, Int) =
      (offset._1 * k, offset._2 * k)

  def apply(history: History, position: Position, toMove: Color):
      Option[Position] = {
    val emptyBetween = (1 until dist).
        forall(i => position(source + mul(offset, i + 1)) == None)

    if (emptyBetween) {
      Move.tryMove(position, toMove, source, source + mul(offset, dist))
    } else {
      None
    }
  }
}

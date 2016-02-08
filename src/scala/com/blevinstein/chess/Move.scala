package com.blevinstein.chess

// There are two types of moves.
// 1. Piece moves. Each living Pawn/Knight/Bishop/Rook/Queen/King may have
//    available moves.
// 2. Castling. Moves two pieces at once, is only allowed under specific
//    conditions. (Any other exceptions?)

trait Move {
  // Returns the new state of the board after making [this] move.
  // Should return None if [this] is not a valid move.
  def apply(position: Position): Option[Position]
  // Returns true if [this] is is a legal move that could be made when the board
  // is in [position].
  // TODO: add post-move checks, e.g. moving King into check
  def isLegal(position: Position): Boolean = apply(position) != None
}

object Move {
  // Create a move, given [input] in chess notation
  def create(position: Position, input: String): Move = input.toCharArray match {
    // kingside castle
    case Array('O', '-', 'O') => Castle(position.toMove, kingside = true)
    // queenside castle
    case Array('O', '-', 'O', '-', 'O') =>
        Castle(position.toMove, kingside = false)
    // e.g. b3
    case Array(fileChar, rankChar)
        if Location.rankToStr.inverse.contains(s"$rankChar") &&
            Location.fileToStr.inverse.contains(s"$fileChar") =>
                infer(position, Location(s"$fileChar$rankChar"))
    // e.g. Nb3
    case Array(pieceChar, fileChar, rankChar)
        if Location.rankToStr.inverse.contains(s"$rankChar") &&
            Location.fileToStr.inverse.contains(s"$fileChar") &&
            Piece.byLetter.contains(s"$pieceChar") =>
                infer(position,
                    Location(s"$fileChar$rankChar"),
                    Piece.byLetter(s"$pieceChar"))
    // TODO:
    // e.g. ab3
    // e.g. Nab3
    // e.g. N2b3
    // e.g. Nb1c3
    case _ => ???
  }

  // Infer a move, given a set of restrictions, such as [dest], [piece],
  // [sourcePredicate].
  def infer(
      position: Position,
      dest: Location,
      piece: Piece = Pawn,
      sourcePredicate: Location => Boolean = (_) => true,
      promote: Option[Piece] = None): Move = {
    require(position(dest) match {
          case None => true
          case Some((color, _)) => color != position.toMove
        },
        "Cannot take a piece of the same color!")
    val candidateMoves =
        position.getMovesFrom(
            // Filter possible source locations based on [sourcePredicate],
            // [toMove], and [piece].
            Location.values.
                filter(sourcePredicate).
                filter((loc) => position(loc) match {
                  case None => false
                  case Some((color, pc)) =>
                      color == position.toMove && pc == piece
                })).
        // Filter to only include moves with the desired effect on [dest].
        filter((move) =>
            move(position).get.apply(dest) == Some(position.toMove, piece))
    require(candidateMoves.length > 0, "No such legal move!")
    require(candidateMoves.length < 2, "Description is ambiguous!")
    candidateMoves(0)
  }

  // Helper methods

  def between(a: Int, b: Int): Range =
      if (a <= b) (a + 1 to b - 1) else (b + 1 to a - 1)

  def firstMove(position: Position, location: Location): Boolean =
      position(location) == Position.initial(location) &&
      position.history.forall{
          case (pos) => pos(location) == Position.initial(location) }

  def allTransformations(offset: (Int, Int)): List[(Int, Int)] = offset match {
    case (f, r) => Set(
        (f, r), (-f, r), (f, -r), (-f, -r),
        (r, f), (-r, f), (r, -f), (-r, -f)).toList
  }

  // Helper for multiplying a tuple of Ints by a scalar
  def mul(offset: (Int, Int), k: Int): (Int, Int) =
      (offset._1 * k, offset._2 * k)

  // Helper for adding tuples of Ints
  // TODO: rm if unused
  //def plus(a: (Int, Int), b: (Int, Int)): (Int, Int) =
  //    (a._1 + b._1, a._2 + b._2)

  // Attempt to move a piece from [source] to [destination].
  // Does not check for occluding pieces in between.
  def tryMove(
      position: Position,
      source: Location,
      dest: Location,
      canCapture: Boolean = true,
      mustCapture: Boolean = false):
      Option[Position] = {
    if (source.isValid && dest.isValid) {
      (position(source), position(dest)) match {
        // Move to open space
        case (Some((sourceColor, piece)), None)
            if !mustCapture =>
            Some(position.update(Map(
                source -> None,
                dest -> Some((sourceColor, piece)))))
        // Capture
        case (Some((sourceColor, piece)), Some((destColor, _)))
            if sourceColor != destColor && canCapture =>
            Some(position.update(Map(
                source -> None,
                dest -> Some((sourceColor, piece)))))
        // Can't move
        case _ => None
      }
    } else {
      None
    }
  }
}

// TODO: test
case class CustomMove(
    source: Location,
    dest: Location,
    canCapture: Boolean = true,
    mustCapture: Boolean = false) extends Move {
  def apply(position: Position): Option[Position] =
      Move.tryMove(
          position,
          source,
          dest,
          canCapture = canCapture,
          mustCapture = mustCapture)
}

object LeaperMove {
  def all(source: Location, offset: (Int, Int)): List[LeaperMove] =
      Move.allTransformations(offset).
          filter(offset => (source + offset).isValid). // bounds check
          map(offset => LeaperMove(source, offset)).
          toList
}
case class LeaperMove(source: Location, offset: (Int, Int)) extends Move {
  require(offset != (0, 0))
  require((source + offset).isValid)

  def apply(position: Position): Option[Position] =
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
  require(offset != (0, 0))
  require(dist > 0)
  require((source + Move.mul(offset, dist)).isValid)

  def apply(position: Position):
      Option[Position] = {
    val emptyBetween = (1 until dist).
        forall(i => position(source + Move.mul(offset, i)) == None)

    if (emptyBetween) {
      Move.tryMove(position, source, source + Move.mul(offset, dist))
    } else {
      None
    }
  }
}

case class Castle(color: Color, kingside: Boolean) extends Move {
  def apply(position: Position): Option[Position] = {
    val rank = if (color == White) 0 else 7
    val kingFile = 4
    val rookFile = if (kingside) 7 else 0

    val newRookFile = if (kingside) 5 else 3
    val newKingFile = if (kingside) 6 else 2

    val emptyBetween = Move.between(kingFile, rookFile).
        forall(i => position(Location(i, rank)) == None)

    if (emptyBetween &&
        Move.firstMove(position, Location(rookFile, rank)) &&
        Move.firstMove(position, Location(kingFile, rank))) {
      Some(position.update(Map(
          Location(kingFile, rank) -> None,
          Location(rookFile, rank) -> None,
          Location(newKingFile, rank) -> Some(color, King),
          Location(newRookFile, rank) -> Some(color, Rook))))
    } else {
      None
    }
  }
}


package com.blevinstein.chess

import com.blevinstein.chess.Location.{strToRank,strToFile}

// There are two types of moves.
// 1. Piece moves. Each living Pawn/Knight/Bishop/Rook/Queen/King may have
//    available moves.
// 2. Castling. Moves two pieces at once, is only allowed under specific
//    conditions. (Any other exceptions?)
//
// Not yet implemented: en passant

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
  def getDest(move: Move, filterCanCapture: Boolean = false): Option[Location] =
      move match {
        case CustomMove(_, dest, canCapture, _) =>
            if (!filterCanCapture || canCapture) Some(dest) else None
        case LeaperMove(source, offset) => Some(source + offset)
        case RiderMove(source, offset, dest) => Some(source + mul(offset, dest))
        case PromotePawn(baseMove, _, _) => getDest(baseMove, filterCanCapture)
        case _ => None
      }

  def createSourcePredicate(str: String): Location => Boolean =
      str.split("") match {
        case Array(rankStr) if strToRank.contains(rankStr) =>
            (loc: Location) => loc.rank == strToRank(rankStr)
        case Array(fileStr) if strToFile.contains(fileStr) =>
            (loc: Location) => loc.file == strToFile(fileStr)
        case Array(fileStr, rankStr)
            if strToRank.contains(rankStr) &&
            strToFile.contains(fileStr) =>
                (loc: Location) =>
                    loc.rank == strToRank(rankStr) &&
                    loc.file == strToFile(fileStr)
      }

  // Create a move, given [input] in chess notation
  def create(position: Position, input: String): Move = input.split("") match {
    // kingside castle
    case Array("O", "-", "O") => Castle(position.toMove, kingside = true)

    // queenside castle
    case Array("O", "-", "O", "-", "O") =>
        Castle(position.toMove, kingside = false)

    // e.g. b3
    case Array(fileStr, rankStr)
        if strToRank.contains(rankStr) &&
        strToFile.contains(fileStr) =>
            infer(position,
                Location(fileStr + rankStr),
                Pawn,
                debugStr = input)

    // e.g. Nb3
    case Array(pieceStr, fileStr, rankStr)
        if strToRank.contains(rankStr) &&
            strToFile.contains(fileStr) &&
            Piece.byLetter.contains(pieceStr) =>
                infer(position,
                    Location(fileStr + rankStr),
                    Piece.byLetter(pieceStr),
                    debugStr = input)

    // e.g. ab3
    case Array(hintStr, fileStr, rankStr)
        if strToRank.contains(rankStr) &&
        strToFile.contains(fileStr) &&
        (strToRank.contains(hintStr) || strToFile.contains(hintStr)) =>
            infer(position,
                Location(fileStr + rankStr),
                Pawn,
                sourcePredicate = createSourcePredicate(hintStr))

    // e.g. Nab3, N2b3
    case Array(pieceStr, hintStr, fileStr, rankStr)
        if strToRank.contains(rankStr) &&
        strToFile.contains(fileStr) &&
        (strToRank.contains(hintStr) || strToFile.contains(hintStr)) &&
        Piece.byLetter.contains(pieceStr) =>
            infer(position,
                Location(fileStr + rankStr),
                Piece.byLetter(pieceStr),
                sourcePredicate = createSourcePredicate(hintStr))

    // e.g. a1Q
    case Array(fileStr, rankStr, promoteStr)
        if List("1", "8").contains(rankStr) &&
        strToFile.contains(fileStr) &&
        Piece.byLetter.contains(promoteStr) &&
        !List("P", "K").contains(promoteStr) =>
            infer(position,
                Location(fileStr + rankStr),
                Pawn,
                promote = Some(Piece.byLetter(promoteStr)))

    // e.g. ab1Q
    case Array(hintStr, fileStr, rankStr, promoteStr)
        if List("1", "8").contains(rankStr) &&
        strToFile.contains(fileStr) &&
        Piece.byLetter.contains(promoteStr) &&
        !List("P", "K").contains(promoteStr) &&
        (strToRank.contains(hintStr) || strToFile.contains(hintStr)) =>
            infer(position,
                Location(fileStr + rankStr),
                Pawn,
                promote = Some(Piece.byLetter(promoteStr)),
                sourcePredicate = createSourcePredicate(hintStr))

    // e.g. Nb1c3
    case Array(pieceStr, sourceFileStr, sourceRankStr, fileStr, rankStr)
        if strToFile.contains(fileStr) &&
        strToFile.contains(sourceFileStr) &&
        strToRank.contains(rankStr) &&
        strToRank.contains(sourceRankStr) &&
        Piece.byLetter.contains(pieceStr) =>
            infer(position,
                Location(fileStr + rankStr),
                Piece.byLetter(pieceStr),
                sourcePredicate =
                    createSourcePredicate(sourceFileStr + sourceRankStr))

    case _ => throw new RuntimeException(s"Unhandled move string: $input")
  }

  // Infer a move, given a set of restrictions, such as [dest], [piece],
  // [sourcePredicate].
  def infer(
      position: Position,
      dest: Location,
      piece: Piece,
      sourcePredicate: Location => Boolean = (_) => true,
      promote: Option[Piece] = None,
      debugStr: String = ""): Move = {
    require(position(dest) match {
          case None => true
          case Some((color, _)) => color != position.toMove
        },
        s"Cannot take a piece of the same color! $debugStr")
    val candidateMoves: List[Move] =
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
        // Filter to only include legal moves
        filter{_.isLegal(position)}.
        // Filter to only include moves with the desired effect on [dest].
        filter((move) =>
            move(position).get.apply(dest) == Some(position.toMove, piece)).
        // Apply promotion effects afterwards if necessary
        map(move => promote match {
          case None => move // TODO: Check for pawns needing to promote?
          case Some(piece) => PromotePawn(move, dest, piece)
        }).toList
    require(candidateMoves.length > 0, s"No such legal move! $debugStr")
    require(candidateMoves.length < 2, s"Description is ambiguous! $debugStr")
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

  def apply(position: Position): Option[Position] = {
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

case class PromotePawn(baseMove: Move, location: Location, newPiece: Piece)
    extends Move {
  require(List(Bishop, Knight, Rook, Queen).contains(newPiece))

  def apply(position: Position): Option[Position] = {
    val basePos: Position = baseMove(position).get
    basePos(location) match {
      case Some((color, Pawn)) =>
          Some(basePos + (location, Some(color, newPiece)))
      case _ => None
    }
  }
}


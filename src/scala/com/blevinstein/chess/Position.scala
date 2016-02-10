package com.blevinstein.chess;

object Position {
  def initial: Position = Position(
      Map(Location(0, 0) -> Some((White, Rook)),
          Location(1, 0) -> Some((White, Knight)),
          Location(2, 0) -> Some((White, Bishop)),
          Location(3, 0) -> Some((White, Queen)),
          Location(4, 0) -> Some((White, King)),
          Location(5, 0) -> Some((White, Bishop)),
          Location(6, 0) -> Some((White, Knight)),
          Location(7, 0) -> Some((White, Rook)),
          Location(0, 1) -> Some((White, Pawn)),
          Location(1, 1) -> Some((White, Pawn)),
          Location(2, 1) -> Some((White, Pawn)),
          Location(3, 1) -> Some((White, Pawn)),
          Location(4, 1) -> Some((White, Pawn)),
          Location(5, 1) -> Some((White, Pawn)),
          Location(6, 1) -> Some((White, Pawn)),
          Location(7, 1) -> Some((White, Pawn)),
          Location(0, 6) -> Some((Black, Pawn)),
          Location(1, 6) -> Some((Black, Pawn)),
          Location(2, 6) -> Some((Black, Pawn)),
          Location(3, 6) -> Some((Black, Pawn)),
          Location(4, 6) -> Some((Black, Pawn)),
          Location(5, 6) -> Some((Black, Pawn)),
          Location(6, 6) -> Some((Black, Pawn)),
          Location(7, 6) -> Some((Black, Pawn)),
          Location(0, 7) -> Some((Black, Rook)),
          Location(1, 7) -> Some((Black, Knight)),
          Location(2, 7) -> Some((Black, Bishop)),
          Location(3, 7) -> Some((Black, Queen)),
          Location(4, 7) -> Some((Black, King)),
          Location(5, 7) -> Some((Black, Bishop)),
          Location(6, 7) -> Some((Black, Knight)),
          Location(7, 7) -> Some((Black, Rook)))
          .withDefaultValue(None),
      White,
      List.empty)

  def create(moveStrings: List[String]): Position =
      moveStrings.foldLeft(Position.initial)
          { case (pos, str) => Move.create(pos, str)(pos).get }
}
// NOTE: [history] is stored in reverse order, so [history.head] is the most
// recent position.
case class Position(
    map: Map[Location, Option[(Color, Piece)]],
    toMove: Color,
    history: List[Position]) {
  import com.blevinstein.chess.TerminalHelper._

  // delegate to [map]
  def apply(location: Location): Option[(Color, Piece)] = map(location)
  def +(kv: (Location, Option[(Color, Piece)])): Position =
      Position(map + kv, toMove, history)

  def update(delta: Map[Location, Option[(Color, Piece)]]): Position = {
      require(!delta.isEmpty)
      Position(delta.foldLeft(map) {
        case (map: Map[Location, Option[(Color, Piece)]],
            kv: (Location, Option[(Color, Piece)])) => map + kv
      }, !toMove, this :: history)
  }

  def rewind: Position = Position(history.head.map, !toMove, history.tail)

  def getCastleMoves: List[Move] =
      List(Castle(White, true),
          Castle(White, false),
          Castle(Black, true),
          Castle(Black, false)).filter{_.isLegal(this)}

  // Gets all moves from pieces at the specified locations.
  def getMovesFrom(locations: List[Location]): List[Move] =
      locations.flatMap((loc: Location) => apply(loc) match {
        case None => None
        case Some((color, piece)) => Some((color, piece, loc))
      }).flatMap{ case (color, piece, loc) => piece.getMoves(this, loc) }

  def getAllMoves: List[Move] =
      getMovesFrom(Location.values) ++ getCastleMoves

  // TODO: rm if unused
  // def getValidMoves: List[Move] = getAllMoves.filter{_.isLegal(this)}

  // TODO: test
  def isAttacking(a: Location, b: Location): Boolean =
      getMovesFrom(List(a)).exists{_.getDest == Some(b)} &&
      apply(a) != None &&
      apply(b) != None &&
      apply(a).get._1 != apply(b).get._1

  // TODO: test
  def isDefending(a: Location, b: Location): Boolean =
      getMovesFrom(List(a)).exists{_.getDest == Some(b)} &&
      apply(a) != None &&
      apply(b) != None &&
      apply(a).get._1 == apply(b).get._1

  // TODO: test
  def getAttackers(location: Location): List[(Color, Piece, Location)] =
      Location.values.filter{isAttacking(_, location)}.flatMap{
            loc => apply(loc) match {
              case Some((color, piece)) => Some(color, piece, loc)
              case None => ???
            }
          }

  // TODO: test
  def getDefenders(location: Location): List[(Color, Piece, Location)] =
      Location.values.filter{isDefending(_, location)}.flatMap{
            loc => apply(loc) match {
              case Some((color, piece)) => Some((color, piece, loc))
              case None => ???
            }
          }

  def getThreatNumber(location: Location): Int =
      getDefenders(location).size - getAttackers(location).size

  // Display functions:

  def prettyPrint: Unit = {
    println(s"To move: $toMove")
    // Blank corner
    print("      ")
    // File labels
    for (file <- 0 until 8) { print(s"  ${Location.fileToStr(file)}   ") }
    println()
    for (rank <- 7 to 0 by -1) {
      // Rank labels
      print(s"  ${Location.rankToStr(rank)}   ")
      for (file <- 0 until 8) {
        Location(file, rank).background match {
          case Black => print(backgroundBlack)
          case White => print(backgroundWhite)
        }
        map(Location(file, rank)) match {
          case None => print("      ")
          case Some((color, piece)) => {
            val threatNumber = getThreatNumber(Location(file, rank))
            color match {
              case Black => print(foregroundBlack)
              case White => print(foregroundWhite)
            }
            print(s" ${getCode(color, piece)} ")
            if (threatNumber > 0) {
              print(foregroundLightGreen)
            } else if (threatNumber == 0) {
              print (foregroundLightYellow)
            } else {
              print(foregroundLightRed)
            }
            print(f"$threatNumber%2d ")
          }
        }
      }
      println(reset)
    }
  }
}

package com.blevinstein.chess;

import com.blevinstein.chess.Piece._

object Position {
  def initial: Position =
      Position(Map(Location(0, 0) -> Some((White, Rook)),
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
          Location(7, 7) -> Some((Black, Rook))).withDefaultValue(None))
}
// TODO: Refactor toMove into Position.
case class Position(map: Map[Location, Option[(Color, Piece)]]) {
  import com.blevinstein.chess.TerminalHelper._

  // delegate to map
  def apply(location: Location): Option[(Color, Piece)] = map(location)
  def +(kv: (Location, Option[(Color, Piece)])) = Position(map + kv)

  def prettyPrint: Unit = {
    // File labels
    print("     ")
    for (file <- 0 until 8) { print(s" ${Location.fileStr(file)}  ") }
    println()
    for (rank <- 0 until 8) {
      // Rank labels
      print(s"  ${Location.rankStr(rank)}  ")
      for (file <- 0 until 8) {
        Location(file, rank).background match {
          case Black => print(backgroundBlack)
          case White => print(backgroundWhite)
        }
        map(Location(file, rank)) match {
          case None => print("    ")
          case Some((color, piece)) => {
            color match {
              case Black => print(foregroundBlack)
              case White => print(foregroundWhite)
            }
            print(s" ${getCode(color, piece)}  ")
          }
        }
      }
      println(reset)
    }
  }
}

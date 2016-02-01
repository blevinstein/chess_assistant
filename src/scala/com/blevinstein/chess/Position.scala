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
object TerminalHelper {
  // used to generate terminal escape sequences
  // http://misc.flogisoft.com/bash/tip_colors_and_formatting
  def escape(codes: List[Int]) = s"\u001b[${codes.map(_.toString).mkString(";")}m"

  val reset = escape(List(0))

  val backgroundWhite = escape(List(48, 5, 246))
  val backgroundBlack = escape(List(48, 5, 240))
  val foregroundWhite = escape(List(97))
  val foregroundBlack = escape(List(30))

  def getCode(color: Color, piece: Piece) = (color, piece) match {
    case (White, King)   => "\u2654"
    case (White, Queen)  => "\u2655"
    case (White, Rook)   => "\u2656"
    case (White, Bishop) => "\u2657"
    case (White, Knight) => "\u2658"
    case (White, Pawn)   => "\u2659"
    case (Black, King)   => "\u265a"
    case (Black, Queen)  => "\u265b"
    case (Black, Rook)   => "\u265c"
    case (Black, Bishop) => "\u265d"
    case (Black, Knight) => "\u265e"
    case (Black, Pawn)   => "\u265f"
    case _ => ???
  }
}
case class Position(map: Map[Location, Option[(Color, Piece)]]) {
  import com.blevinstein.chess.TerminalHelper._

  def prettyPrint: Unit = {
    // File labels
    print("     ")
    for (file <- 0 until 8) { print(s"  ${Location.fileStr(file)}  ") }
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
          case None => print("     ")
          case Some((color, piece)) => {
            color match {
              case Black => print(foregroundBlack)
              case White => print(foregroundWhite)
            }
            print(s"  ${getCode(color, piece)}  ")
          }
        }
      }
      println(reset)
    }
  }
}

package com.blevinstein.chess

object Piece {
  val Pawn = Piece("P")
  val Knight = Piece("N")
  val Bishop = Piece("B")
  val Rook = Piece("R")
  val Queen = Piece("Q")
  val King = Piece("K")
}
case class Piece(letter: String)

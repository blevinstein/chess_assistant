package com.blevinstein.chess

object TerminalHelper {
  // used to generate terminal escape sequences
  // http://misc.flogisoft.com/bash/tip_colors_and_formatting
  def escape(codes: List[Int]) = s"\u001b[${codes.map(_.toString).mkString(";")}m"

  val reset = escape(List(0))

  val backgroundWhite = escape(List(48, 5, 246))
  val backgroundBlack = escape(List(48, 5, 240))

  val foregroundWhite  = escape(List(97))
  val foregroundBlack  = escape(List(30))
  val foregroundRed    = escape(List(31))
  val foregroundGreen  = escape(List(32))
  val foregroundYellow = escape(List(33))

  val foregroundLightRed    = escape(List(91))
  val foregroundLightGreen  = escape(List(92))
  val foregroundLightYellow = escape(List(93))

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

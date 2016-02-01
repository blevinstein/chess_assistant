package com.blevinstein.chess

abstract class Color {
  def unary_! : Color = this match {
    case White => Black
    case Black => White
  }
}
object White extends Color
object Black extends Color

package com.blevinstein.chess

abstract class Color {
  def unary_! : Color = this match {
    case White => Black
    case Black => White
  }

  override def toString: String = this match {
    case White => "White"
    case Black => "Black"
  }
}
object White extends Color
object Black extends Color

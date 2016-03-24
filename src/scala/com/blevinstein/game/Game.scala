package com.blevinstein.game

/**
 * Represents a [Game] as described in [On Numbers and Games] by John Conway
 */
object Game {
  def apply(i: Int): Game = if (i == 0) {
    Game(List(), List())
  } else if (i > 0) {
    Game(List(Game(i-1)), List())
  } else {
    Game(List(), List(Game(i+1)))
  }

  def approx[T](state: T, maxDepth: Int)
      (implicit expand: T => (List[T], List[T], Game)) = ???
}
case class Game(left: List[Game], right: List[Game]) {
  def unary_- : Game = Game(right.map((r) => -r), left.map((l) => -l))

  def >=(other: Game): Boolean =
      !right.exists((r) => r <= other) && !other.left.exists((l) => this <= l)
  def <=(other: Game): Boolean = other >= this

  def ==(other: Game): Boolean = this >= other && this <= other

  def >(other: Game): Boolean = this >= other && !(this <= other)
  def <(other: Game): Boolean = other > this

  def :>(other: Game): Boolean = !(this <= other)
  def :<(other: Game): Boolean = !(this >= other)

  def +(other: Game): Game = Game(
      left.map((l) => l + other) ++ other.left.map((l) => l + this),
      right.map((r) => r + other) ++ other.right.map((r) => r + this))
  def -(other: Game): Game = this + (-other)

  def isNumber: Boolean = !left.exists((l) => right.exists((r) => l >= r))

  def toFloat: Option[Float] = ???
}


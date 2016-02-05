package com.blevinstein.util

// Implementation partially copied from:
// http://stackoverflow.com/questions/9850786/is-there-such-a-thing-as-bidirectional-maps-in-scala

class BiMap[X, Y](map: Map[X, Y]) {
  def this(tuples: (X, Y)*) = this(tuples.toMap)

  val inverse = map map (_.swap)
  require(map.size == inverse.size)

  // delegate to Map
  def apply(x: X): Y = map(x)
  def size: Int = map.size
}

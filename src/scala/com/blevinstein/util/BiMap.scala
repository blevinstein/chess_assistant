package com.blevinstein.util

// Implementation partially copied from:
// http://stackoverflow.com/questions/9850786/is-there-such-a-thing-as-bidirectional-maps-in-scala

case class BiMap[X, Y](kvs: (X, Y)*) {
  val map = Map(kvs:_*)
  val inverse = Map(kvs.map{_.swap}:_*)

  // no duplicate keys OR values allowed
  require(map.size == inverse.size)

  // delegate to Map
  def apply(x: X): Y = map(x)
  def size: Int = map.size
}

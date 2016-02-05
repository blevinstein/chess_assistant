package com.blevinstein.util

import org.scalatest._

class BiMapTest extends FunSuite with Matchers {
  test("BiMap basics") {
    val b = new BiMap("a" -> 1, "b" -> 2)
    b("a") shouldEqual 1
    b.inverse(2) shouldEqual "b"
  }
  test("BiMap same type") {
    val b = new BiMap(1 -> 2, 2 -> 3)
    b(1) shouldEqual 2
    b.inverse(3) shouldEqual 2
  }
  test("BiMap fails with duplicate values") {
    an [IllegalArgumentException] should be thrownBy
        new BiMap(1 -> 2, 2 -> 2).inverse
  }
}

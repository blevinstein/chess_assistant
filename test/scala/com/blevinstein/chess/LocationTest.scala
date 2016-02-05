package com.blevinstein.chess

import org.scalatest._

class LocationTest extends FunSuite with Matchers {
  test("constructor") {
    Location("a1") shouldEqual Location(0, 0)
    Location("a8") shouldEqual Location(0, 7)
    Location("h8") shouldEqual Location(7, 7)
    Location("h1") shouldEqual Location(7, 0)
  }

  test("isValid") {
    Location(0, -1).isValid shouldEqual false
    Location(0, 0).isValid shouldEqual true
    Location(0, 7).isValid shouldEqual true
    Location(0, 8).isValid shouldEqual false

    Location(-1, 0).isValid shouldEqual false
    Location(0, 0).isValid shouldEqual true
    Location(7, 0).isValid shouldEqual true
    Location(8, 0).isValid shouldEqual false
  }

  test("values") {
    Location.values.contains(Location("a1")) shouldEqual true
    Location.values.contains(Location("h8")) shouldEqual true
    Location.values.contains(Location("d4")) shouldEqual true
    Location.values.length shouldEqual 64
  }

  test("background") {
    Location("a1").background shouldEqual Black
  }

  test("+") {
    Location("b2") + (0, 0) shouldEqual Location("b2")
    Location("b2") + (1, 2) shouldEqual Location("c4")
  }

  test("toString") {
    for (location <- Location.values) {
      Location(location.toString) shouldEqual location
    }
  }
}

package com.blevinstein.game

import org.scalatest._

class GameTest extends FunSuite with Matchers {
  test("isNumber") {
    Game(0).isNumber shouldEqual true
    Game(1).isNumber shouldEqual true
    Game(List(Game(1)), List(Game(-1))).isNumber shouldEqual false

    (Game(1) + Game(2) == Game(3)) shouldEqual true
  }
}


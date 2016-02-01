package com.blevinstein.chess;

object Repl extends App with Runnable {
  def run: Unit = {
    var currentPosition = Position.initial
    var toMove = White

    currentPosition.prettyPrint
  }

  new Thread(this).start
}

package com.blevinstein.chess;

import com.blevinstein.chess.TerminalHelper._

import scala.io.StdIn

object Repl extends App with Runnable {
  def run: Unit = {
    var history = List.empty
    var currentPosition = Position.initial

    while (true) {
      currentPosition.prettyPrint

      println(s"to move: ${currentPosition.toMove}")

      print("move > ")
      val moveStr = StdIn.readLine()
      // TODO check for commands

      try {
        val move = Move.infer(currentPosition, moveStr)
      } catch {
        case e: Throwable => println(s"$foregroundRed$e$reset")
      }
    }
  }

  new Thread(this).start
}

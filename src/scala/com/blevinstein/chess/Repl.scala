package com.blevinstein.chess;

import com.blevinstein.chess.TerminalHelper._

import scala.io.StdIn

object Repl extends App with Runnable {
  def run: Unit = {
    var currentPosition = Position.initial

    while (true) {
      currentPosition.prettyPrint
      print("move > ")
      val line = StdIn.readLine()
      // TODO add commands

      for (moveStr <- line.split(" ")) {
        try {
          val move = Move.create(currentPosition, moveStr)
          currentPosition = move(currentPosition).right.get
        } catch {
          case e: Throwable => {
            print(foregroundRed)
            e.printStackTrace()
            print(reset)
          }
        }
      }
    }
  }

  new Thread(this).start
}

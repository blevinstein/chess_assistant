package com.blevinstein.chess

import org.scalatest._
import spray.json._

class ChessServerTest extends FunSuite with Matchers {
  import ChessJsonProtocol._

  test("ColorFormat") {
    White.asInstanceOf[Color].toJson shouldEqual JsString("white")
    Black.asInstanceOf[Color].toJson shouldEqual JsString("black")

    JsString("white").convertTo[Color] shouldEqual White
    JsString("black").convertTo[Color] shouldEqual Black
  }

  test("PieceFormat") {
    for (piece <- Piece.byLetter.values) {
      piece.toJson shouldEqual JsString(piece.letter)
      JsString(piece.letter).convertTo[Piece] shouldEqual piece
    }
  }

  test("LocationFormat") {
    for (location <- Location.values) {
      location.toJson shouldEqual JsString(location.toString)
      JsString(location.toString).convertTo[Location] shouldEqual location
    }
  }
}

package com.blevinstein.chess

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.event.Logging
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import spray.can.Http
import spray.http._
import spray.http.HttpMethods._
import spray.http.Uri
import spray.httpx.SprayJsonSupport
import spray.json._
import spray.routing.HttpService
import spray.util.LoggingContext

object ChessJsonProtocol extends DefaultJsonProtocol {
  implicit object ColorFormat extends RootJsonFormat[Color] {
    def read(json: JsValue): Color = json match {
      case JsString("black") => Black
      case JsString("white") => White
    }
    def write(color: Color): JsValue = color match {
      case Black => JsString("black")
      case White => JsString("white")
    }
  }

  implicit object LocationFormat extends RootJsonFormat[Location] {
    def read(json: JsValue): Location = json match {
      case JsString(str) => Location(str)
    }
    def write(loc: Location): JsValue = JsString(loc.toString)
  }

  implicit object PieceFormat extends RootJsonFormat[Piece] {
    def read(json: JsValue): Piece = json match {
      case JsString(letter) => Piece.byLetter(letter)
    }
    def write(piece: Piece): JsValue = JsString(piece.letter)
  }

  implicit val colorPieceFormat: JsonFormat[(Color, Piece)] =
      tuple2Format[Color, Piece]
  implicit val positionFormat: JsonFormat[Position] =
      jsonFormat3(Position.apply) // map, toMove, history
}

class ChessServlet extends Actor with HttpService {
  import SprayJsonSupport._
  import ChessJsonProtocol._

  def actorRefFactory = context

  def receive = runRoute(chessService)

  val chessService =
      pathEndOrSingleSlash {
        get {
          getFromFile("src/html/index.html")
        }
      } ~
      path("new-board") {
        get {
          complete {
            Position.initial.toJson.toString
          }
        }
      }
}

object ChessServer extends App {
  implicit val system = ActorSystem()
  implicit val timeout = Timeout(5.seconds)
  val listener = system.actorOf(Props[ChessServlet], name = "ChessServlet")
  IO(Http) ? Http.Bind(listener, interface = "localhost", port = 8080)
}

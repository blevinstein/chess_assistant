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
import spray.http.CacheDirectives._
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import spray.http.Uri
import spray.httpx.SprayJsonSupport._
import spray.json._
import spray.routing.Directive1
import spray.routing.HttpService
import spray.util.LoggingContext

object ChessJsonProtocol extends DefaultJsonProtocol {

  implicit object ColorFormat extends RootJsonFormat[Color] {
    def read(json: JsValue): Color = json match {
      case JsString("black") => Black
      case JsString("white") => White
      case _ => ???
    }
    def write(color: Color): JsValue = color match {
      case Black => JsString("black")
      case White => JsString("white")
    }
  }

  implicit object LocationFormat extends RootJsonFormat[Location] {
    def read(json: JsValue): Location = json match {
      case JsString(str) => Location(str)
      case _ => ???
    }
    def write(loc: Location): JsValue = JsString(loc.toString)
  }

  implicit object PieceFormat extends RootJsonFormat[Piece] {
    def read(json: JsValue): Piece = json match {
      case JsString(letter) => Piece.byLetter(letter)
      case _ => ???
    }
    def write(piece: Piece): JsValue = JsString(piece.letter)
  }

  implicit object MoveFormat extends RootJsonFormat[Move] {
    def read(json: JsValue): Move = ???
    def write(move: Move): JsValue = JsObject(
        "source" -> Move.getSource(move).toJson,
        "dest" -> Move.getDest(move).toJson)
  }

  implicit object PositionFormat extends RootJsonFormat[Position] {
    def read(json: JsValue): Position = json match {
      case JsObject(props) =>
        Position(
            props("map").convertTo[Map[Location, Option[(Color, Piece)]]].
                withDefaultValue(None),
            props("toMove").convertTo[Color],
            props("history").convertTo[List[Position]])
      case _ => ???
    }
    def write(position: Position): JsValue = JsObject(
        "map" -> position.map.toJson,
        "toMove" -> position.toMove.toJson,
        "history" -> position.history.toJson)
  }

  implicit val colorPieceFormat: JsonFormat[(Color, Piece)] =
      tuple2Format[Color, Piece]
}

import ChessJsonProtocol._

class ChessServlet extends Actor with HttpService {

  def actorRefFactory = context

  def receive = runRoute(chessService)

  val chessService =
      pathEndOrSingleSlash {
        respondWithHeaders(`Cache-Control`(`no-cache`, `no-store`, `must-revalidate`)) {
          get {
            getFromFile("src/html/v2/index.html")
          }
        }
      } ~
      path("new-board") {
        get {
          complete {
            Position.initial.toJson.toString
          }
        }
      } ~
      path("get-all-moves") {
        post {
          extract(_.request.entity.asString.parseJson.convertTo[Position]) {
            position => complete {
              position.getAllMoves.toJson.toString
            }
          }
        }
      } ~
      path("get-moves") {
        post {
          extract(_.request.entity.asString.parseJson.asJsObject) { jsObj => {
            val position = jsObj.fields("position").convertTo[Position]
            val location = jsObj.fields("source").convertTo[Location]
            complete {
              position.getMovesFrom(List(location)).toJson.toString
            }
          }}
        }
      } ~
      // Search in these directories
      getFromDirectory("src/html/v2") ~
      getFromDirectory("src/html/v1") ~ // Fallback to v1
      getFromDirectory("src/html/bower_components")
}

object ChessServer extends App {
  implicit val system = ActorSystem()
  implicit val timeout = Timeout(5.seconds)
  val listener = system.actorOf(Props[ChessServlet], name = "ChessServlet")
  IO(Http) ? Http.Bind(listener, interface = "localhost", port = 8080)
}

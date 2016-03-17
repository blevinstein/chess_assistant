package com.blevinstein.chess

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
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

  implicit object InvalidReasonFormat extends RootJsonFormat[InvalidReason] {
    def read(json: JsValue): InvalidReason = ???
    def write(reason: InvalidReason): JsValue = reason match {
      case CantCapture => JsString("Can't capture")
      case MustCapture => JsString("Must capture")
      case SameColor => JsString("Can't capture same color")
      case NoPieceAtSource => JsString("No piece at source")
      case WrongColorToMove => JsString("Wrong color to move")
      case WrongPiece(expected, actual) =>
          JsString(s"Expected $expected but was $actual")
      case OccludedBy(pieces) => JsString(s"Path blocked by $pieces")
      case HasMoved(location) =>
          JsString(s"Piece at $location has already moved.")
      case InvalidArg(arg) => JsString(s"Invalid arg: $arg")
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
        "history" ->
            position.history.map{p => Position(p.map, p.toMove, List())}.toJson)
  }

  implicit object MoveFormat extends RootJsonFormat[Move] {
    def read(json: JsValue): Move = {
      val jsObj = json.asJsObject
      val position = jsObj.fields("position").convertTo[Position]
      val source = jsObj.fields("source").convertTo[Location]
      val dest = jsObj.fields("dest").convertTo[Location]
      Move.find(position, source, dest)
    }
    def write(move: Move): JsValue = JsObject(
        "source" -> move.source.toJson,
        "dest" -> move.dest.toJson)
  }

  implicit val colorPieceFormat: JsonFormat[(Color, Piece)] =
      tuple2Format[Color, Piece]
}


class ChessServlet extends Actor with HttpService {
  implicit def actorRefFactory = context

  def receive = runRoute(chessService)

  val chessService = {
    import ChessJsonProtocol._

    // entrypoint
    pathEndOrSingleSlash {
      respondWithHeaders(`Cache-Control`(`no-cache`, `no-store`, `must-revalidate`)) {
        get {
          getFromFile("src/html/v2/index.html")
        }
      }
    } ~
    /**
      * GET new-board
      * response: Position
      * Returns the initial state of a board.
      */
    path("new-board") {
      get {
        complete {
          Position.initial
        }
      }
    } ~
    /**
      * POST get-all-moves (request: Position)
      * response: List[Move]
      * Get all moves originating at [source].
      */
    path("get-all-moves") {
      post {
        extract(_.request.entity.asString.parseJson.convertTo[Position]) {
          position => complete {
            position.getAllMoves
          }
        }
      }
    } ~
    /**
      * POST get-moves (request: { position: Position, source: Location })
      * response: List[Move]
      * Get all moves originating at [source].
      */
    path("get-moves") {
      post {
        extract(_.request.entity.asString.parseJson.asJsObject) { jsObj => {
          val position = jsObj.fields("position").convertTo[Position]
          val source = jsObj.fields("source").convertTo[Location]
          complete {
            position.getMovesFrom(List(source))
          }
        }}
      }
    } ~
    /**
      * POST make-move (request: {
      *     position: Position,
      *     source: Location,
      *     dest: Location,
      *     [promote: Piece] })
      * response: Position
      * Return the state of the board after making [move].
      */
    path("make-move") {
      post {
        extract(_.request.entity.asString.parseJson.asJsObject) { jsObj => {
          val position = jsObj.fields("position").convertTo[Position]
          val move = jsObj.convertTo[Move]
          complete {
            move(position) match {
              case Left(reason) => JsObject(Map("error" -> reason.toJson))
              case Right(position) => position
            }
          }
        }}
      }
    } ~
    /**
     * POST is-legal (request: {
     *    position: Position,
     *    source: Location,
     *    dest: Location,
     *    [promote: Piece] })
     * response: {success: Boolean, [reason: InvalidReason]}
     * Return whether a move is valid, including a reason if it is invalid.
     */
    path("is-legal") {
      post {
        extract(_.request.entity.asString.parseJson.asJsObject) { jsObj => {
          val position = jsObj.fields("position").convertTo[Position]
          val move = jsObj.convertTo[Move]
          complete {
            move(position) match {
              case Right(_) => JsObject(Map("success" -> JsTrue))
              case Left(reason) =>
                  JsObject(Map("success" -> JsFalse, "reason" -> reason.toJson))
            }
          }
        }}
      }
    } ~
    // Search in these directories for resources
    respondWithHeaders(`Cache-Control`(`no-cache`, `no-store`, `must-revalidate`)) {
      getFromDirectory("src/html/v2") ~
      getFromDirectory("src/html/bower_components")
    }
  }
}

object ChessServer extends App {
  implicit val system = ActorSystem()
  implicit val timeout = Timeout(5.seconds)
  val listener = system.actorOf(Props[ChessServlet], name = "ChessServlet")
  IO(Http) ? Http.Bind(listener, interface = "localhost", port = 8080)
}

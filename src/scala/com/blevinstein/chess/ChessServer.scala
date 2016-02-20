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
import spray.http.HttpMethods._
import spray.http.Uri
import spray.httpx.SprayJsonSupport._
import spray.routing.HttpService

sealed trait ChessMessage

class ChessServlet extends Actor with HttpService {
  def actorRefFactory = context
  def receive = runRoute {
    pathEndOrSingleSlash {
      get {
        getFromFile("src/html/index.html")
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

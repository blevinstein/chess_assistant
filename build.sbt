scalaVersion := "2.11.7"


resolvers += "spray repo" at "http://repo.spray.io"


libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.3",
    "io.spray" %% "spray-http" % "1.3.3",
    "io.spray" %% "spray-io" % "1.3.3",
    "io.spray" %% "spray-routing" % "1.3.3",
    "io.spray" %% "spray-util" % "1.3.3",
    "com.typesafe.akka" %% "akka-actor" % "2.3.9")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"


scalaSource in Compile := baseDirectory.value / "src" / "scala"

scalaSource in Test := baseDirectory.value / "test" / "scala"


scalacOptions ++= Seq("-deprecation", "-feature", "-Xlog-implicits")

lazy val repl = taskKey[Unit]("Run chess repl.")

repl := { (runMain in Compile).toTask(" com.blevinstein.chess.Repl").value }

lazy val server = taskKey[Unit]("Run chess server.")

server := {
  (runMain in Compile).toTask(" com.blevinstein.chess.ChessServer").value
}


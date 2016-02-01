scalaVersion := "2.11.7"


libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"


scalaSource in Compile := baseDirectory.value / "src" / "scala"

scalaSource in Test := baseDirectory.value / "test" / "scala"


scalacOptions ++= Seq("-deprecation", "-feature", "-Xlog-implicits")

lazy val repl = taskKey[Unit](
  "Run chess repl.")

repl := { (runMain in Compile).toTask(" com.blevinstein.chess.Repl").value }


name := "Battleship"

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1"
libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.13.4"

mainClass in (Compile,run) := Some("com.battleship.Battleship")
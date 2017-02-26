name := "shaping-the-future"

scalaVersion in ThisBuild := "2.12.1"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.0.4"
, "io.swagger" % "swagger-parser" % "1.0.25"
, "org.scala-lang" % "scala-compiler" % scalaVersion.value
, "org.scalameta" %% "scalameta" % "1.6.0"
)

val circeVersion = "0.7.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

scalacOptions ++= Seq("-deprecation", "-feature", "-language:higherKinds", "-language:postfixOps")

initialCommands in console := """
import Presentation._
println("':load present.txt' to start the presentation!")
"""

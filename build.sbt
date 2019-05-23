name := "guardrail-talk"

scalaVersion in ThisBuild := "2.12.8"

val akkaVersion       = "10.0.14"
val catsVersion       = "1.5.0"
val circeVersion      = "0.11.1"
val scalatestVersion  = "3.0.7"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"         % akkaVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaVersion,
  "io.circe"          %% "circe-core"        % circeVersion,
  "io.circe"          %% "circe-generic"     % circeVersion,
  "io.circe"          %% "circe-java8"       % circeVersion,
  "io.circe"          %% "circe-parser"      % circeVersion,
  "org.scalatest"     %% "scalatest"         % scalatestVersion % Test,
  "org.typelevel"     %% "cats-core"         % catsVersion
)

initialCommands in console := """
import Presentation._
println("':load present.txt' to start the presentation!")
"""

scalacOptions ++= Seq(
    "-deprecation"            // Emit warning and location for usages of deprecated APIs
  , "-encoding", "UTF-8"      // Specify character encoding used by source files
  , "-feature"                // Emit warning and location for usages of features that should be imported explicitly
  , "-target:jvm-1.8"         // Target platform for object files
  , "-unchecked"              // Enable additional warnings where generated code depends on assumptions
)

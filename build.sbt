name := "shaping-the-future"

scalaVersion in ThisBuild := "2.12.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.7"

scalacOptions ++= Seq("-deprecation", "-feature", "-language:higherKinds", "-language:postfixOps")

initialCommands in console := """
import Presentation._
println("':load present.txt' to start the presentation!")
"""

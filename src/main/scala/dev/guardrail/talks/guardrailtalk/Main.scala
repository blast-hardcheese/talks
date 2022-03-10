package dev.guardrail.talks.guardrailtalk

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def run(args: List[String]) =
    GuardrailtalkServer.stream[IO].compile.drain.as(ExitCode.Success)
}

package dev.guardrail.talks.guardrailtalk

import cats.effect.IO
import cats.effect.{Async, Resource}
import cats.syntax.all._

import com.comcast.ip4s._

import fs2.Stream

import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.Logger

import dev.guardrail.example.routes

object GuardrailtalkServer {

  def stream[F[_]: Async]: Stream[F, Nothing] = {
    for {
      _ <- Stream.resource(EmberClientBuilder.default[F].build)

      httpApp = (
        new routes.Resource[F]().routes(new PetRoutes[F])
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      exitCode <- Stream.resource(
        EmberServerBuilder.default[F]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(finalHttpApp)
          .build >>
        Resource.eval(Async[F].never)
      )
    } yield exitCode
  }.drain
}

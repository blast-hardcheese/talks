package dev.guardrail.talks.guardrailtalk

import cats.effect.IO
import cats.effect.{Async, Resource}
import cats.syntax.all._

import com.comcast.ip4s._

import fs2.Stream

import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.Logger

import com.icanhazdadjoke.joke.JokeClient
import dev.guardrail.example.routes

object GuardrailtalkServer {

  def stream[F[_]: Async]: Stream[F, Nothing] = {
    for {
      emberClient <- Stream.resource(EmberClientBuilder.default[F].build)

      jokeClient = JokeClient.httpClient[F](Client.apply({ req =>
        import org.http4s.MediaRange
        import org.http4s.headers.Accept
        import org.http4s.headers.MediaRangeAndQValue
        // Forcibly inject `Accept: application/*` header to work around https://github.com/guardrail-dev/guardrail/issues/1424
        val newRequest = req.withHeaders(Accept(MediaRangeAndQValue.withDefaultQValue(MediaRange.`application/*`)))
        emberClient.run(newRequest)
      }))

      httpApp = (
        new routes.Resource[F]().routes(new PetRoutes[F](jokeClient))
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

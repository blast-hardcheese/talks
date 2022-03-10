package dev.guardrail.talks.guardrailtalk

import cats.effect.IO
import io.circe.Json

import io.circe.Json
import munit.CatsEffectSuite

import org.http4s._
import org.http4s.implicits._
import org.http4s.circe._

import dev.guardrail.example

class HelloWorldSpec extends CatsEffectSuite {

  test("HelloWorld returns status code 200") {
    assertIO(retHelloWorld.map(_.status) ,Status.Ok)
  }

  test("HelloWorld returns hello world message") {
    assertIO(retHelloWorld.flatMap(_.as[Json]), Json.arr(
      Json.obj("id" -> Json.fromLong(1L), "name" -> Json.fromString("Fluffy"), "tag" -> Json.fromString("soft")),
      Json.obj("id" -> Json.fromLong(2L), "name" -> Json.fromString("Fido"), "tag" -> Json.fromString("bitey")),
    ))
  }

  private[this] val service: HttpApp[IO] =
    new example.routes.Resource[IO]().routes(new PetRoutes[IO]).orNotFound

  private[this] val retHelloWorld: IO[Response[IO]] = {
    service(Request[IO](Method.GET, uri"/pets"))
  }
}

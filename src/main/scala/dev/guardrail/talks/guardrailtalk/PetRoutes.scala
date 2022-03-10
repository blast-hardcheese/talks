package dev.guardrail.talks.guardrailtalk

import cats.effect.{ IO, Sync }
import cats.implicits._

import dev.guardrail.example.routes.{ Handler, Resource }

class PetRoutes[F[_]: Sync] extends Handler[F] {
  def listPets(respond: Resource.ListPetsResponse.type)(): F[Resource.ListPetsResponse] = respond.Ok("Fluffy, Fido").pure[F].widen
}

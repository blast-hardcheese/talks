package dev.guardrail.talks.guardrailtalk

import cats.effect.{ IO, Sync }
import cats.implicits._

import dev.guardrail.example.routes.{ Handler, Resource }
import dev.guardrail.example.routes.definitions.Pet

class PetRoutes[F[_]: Sync] extends Handler[F] {
  def listPets(respond: Resource.ListPetsResponse.type)(tag: Option[Iterable[String]]): F[Resource.ListPetsResponse] = respond.Ok(
    Vector(
      Pet(id=1L, "Fluffy", Some("soft")),
      Pet(id=2L, "Fido", Some("bitey"))
    ).filter(pet => tag.forall(_.exists(pet.tag.contains)))
  ).pure[F].widen
}

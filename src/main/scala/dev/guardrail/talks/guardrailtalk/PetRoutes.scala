package dev.guardrail.talks.guardrailtalk

import cats.effect.{ IO, Sync }
import cats.implicits._

import com.icanhazdadjoke.joke.JokeClient
import dev.guardrail.example.routes.definitions.{ Pet, PetNotFoundResponse }
import dev.guardrail.example.routes.{ Handler, Resource }
import com.icanhazdadjoke.definitions.Joke

class PetRoutes[F[_]: Sync](jokeClient: JokeClient[F]) extends Handler[F] {
  val pets = Vector(
    Pet(id=1L, "Fluffy", Some("soft")),
    Pet(id=2L, "Fido", Some("bitey"))
  )

  def getPet(respond: Resource.GetPetResponse.type)(id: Long): F[Resource.GetPetResponse] = {
    val notFound = for {
      resp <- jokeClient.getJoke()
    } yield resp.fold(
      handleOk={ case Joke(joke) =>
        respond.NotFound(PetNotFoundResponse(s"No pet found with id=${id}", Some(joke)))
      },
      handleNotFound=respond.NotFound(PetNotFoundResponse(s"No pet found with id=${id}", None))
    )
    pets
      .filter(_.id == id)
      .headOption
      .fold[F[Resource.GetPetResponse]](notFound.widen)(pet => respond.Ok(pet).pure[F].widen)
  }

  def listPets(respond: Resource.ListPetsResponse.type)(tag: Option[Iterable[String]]): F[Resource.ListPetsResponse] = respond.Ok(
    pets.filter(pet => tag.forall(_.exists(pet.tag.contains)))
  ).pure[F].widen
}

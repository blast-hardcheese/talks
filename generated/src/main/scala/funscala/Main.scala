package funscala

import scala.concurrent.Future

import funscala.generated.Resource.LoginResponse

object GuardrailApp extends App {
  implicit val system = akka.actor.ActorSystem()

  val handler = new generated.Handler {
    def login(
        respond: LoginResponse.type
    )(
        username: String,
        password: String
    ): Future[LoginResponse] =
      (username, password) match {
        case ("functional", "scala") =>
          val preseed = funscala.generated.definitions.LoginPreseed()
          Future.successful(respond.OK(preseed))
        case _ =>
          Future.successful(respond.Forbidden)
      }
  }

  // Generate routing layer
  val routes: akka.http.scaladsl.server.Route = generated.Resource.routes(handler)
  // Just call your function
  val plain: Future[LoginResponse] = handler.login(LoginResponse)("functional", "scala")
}

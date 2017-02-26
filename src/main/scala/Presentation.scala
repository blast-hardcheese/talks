
object Presentation {
  val specStringExtra = """
      put:
        operationId: updateUser
        parameters:
          -
            in: query
            name: id
            type: number
          -
            in: body
            name: user
            schema:
              $ref: '#/definitions/User'
        responses:
          200:
            schema:
              $ref: '#/definitions/User'

    /user:
      post:
        operationId: createUser
        parameters:
          in: body
          name: user
          schema:
            $ref: '#/definitions/User'
        responses:
          200:
            schema:
              $ref: '#/definitions/User'
  definitions:
    User:
      type: object
      required:
        - name
        - age
      properties:
        id:
          type: number
          format: int64
        name:
          type: string
        age:
          type: number
          format: int32
  """

import _root_.io.swagger.parser._

val specString = """
swagger: 2.0
host: localhost
schemes:
  - http

paths:
  /user/{id}:
    get:
      operationId: getUser
      produces:
      - application/json
      parameters:
        in: query
        name: id
        type: number
        format: int64
      responses:
        200:
          schema:
            $ref: '#/definitions/User'
"""

val swagger = new SwaggerParser().parse(specString ++ specStringExtra)


import scala.collection.JavaConverters._

val scheme = swagger.getSchemes.asScala.head.name.toLowerCase
// scheme: String = http
val host = swagger.getHost
// host: String = localhost


val (pathTpl, pathSpec) = swagger.getPaths.asScala.toList.head
// pathTpl: String = /user/{id}
// pathSpec: io.swagger.models.Path =
//   io.swagger.models.Path@7acc05cf


val operationSpec = pathSpec.getOperations.asScala.toList.head
// operationSpec: io.swagger.models.Operation =
//   io.swagger.models.Operation@e709de8e[null]


val operationId = operationSpec.getOperationId
// operationId: String = getUser


val (statusCode, responseSpec) =
  operationSpec.getResponses.asScala.toList.head
// statusCode: String = 200
// responseSpec: io.swagger.models.Response =
//   io.swagger.models.Response@f36aca24


import _root_.io.swagger.models.properties.RefProperty
val responseType = responseSpec.getSchema.asInstanceOf[RefProperty]
// responseType: io.swagger.models.properties.RefProperty =
//   io.swagger.models.properties.RefProperty@6522085c


val responseClassName = responseType.getSimpleRef
// responseClassName: String = User


import scala.collection.immutable.Seq
// Renaming here to work around a bug in the slides. Not needed normally.
import scala.meta.{XtensionQuasiquoteTerm => Quasiquote, _}


val imports: Seq[Stat] = source"""
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpRequest
import akka.stream.ActorMaterializer
import _root_.io.circe.{Decoder, Error, jawn}

import scala.concurrent.Future
import scala.concurrent.duration.Duration
""".stats


def buildMethod(scheme: Lit, host: Lit, pathTpl: Lit, operationId: Term.Name,
    bodytpe: Type, params: Seq[Term.Param]): Defn.Def =
  q"""
  def $operationId(..$params)(implicit as: ActorSystem, mat: ActorMaterializer,
      dec: Decoder[$bodytpe], timeout: Duration
    ): Future[Either[Error, $bodytpe]] = {

    val uri = ( $scheme + "://" + $host +
      ${params.foldLeft[Term](pathTpl) { case (a, param) =>
        q"""$a.replace("{" + ${Lit(param.name.value)} + "}",
                       ${Term.Name(param.name.value)})
        """ }}
    )

    Http().singleRequest(HttpRequest(uri = uri))
      .flatMap(_.entity.toStrict(timeout).map(jawn.decode[$bodytpe](_)))
  }
  """

}

class: title

<center>
<h1>Specification-First HTTP Services<br />With <b>guardrail</b></h1>
</center>

<center><a href="https://guardrail.dev/" style="color: #0000AA; font-size: xx-large">https://guardrail.dev</a></center>

--



```
  Name:  Devon Stewart

     X: @blast_hardchese
GitHub:  blast-hardcheese

 Email:  devon@guardrail.dev
```
???
A bit about me
---
class: biglist

## What problems we intend to solve
--

1. Ambiguity reflecting spec out of code
???
Did everything important get captured? How do we know?

---
class: biglist

## What problems we intend to solve

1. Ambiguity reflecting spec out of code
2. Leaky abstractions
???
Your business domain is described by data, the HTTP domain is vastly larger, described by an entire specification.

For the happy path, I just want to write a simple function that deals with my domain objects.

I want symbols representing the relevant parts of the HTTP domain:
- "OK vs 200"
- `fold` over possible responses, decoded response bodies

---
class: biglist

## What problems we intend to solve

1. Ambiguity reflecting spec out of code
2. Leaky abstractions
3. Duplication of effort
???
spending a lot of time on internal abstractions is hard to justify in small teams, leading to tight coupling to the HTTP library and overall less testable code.

---
class: biglist

.left-column[
## Advantages of spec&#8209;first
]

.right-column[
1. Promotes collaboration
1. Plug-and-play with existing ecosystem
1. Type system verifies correctness instead of tests
]
???
design APIs before implementation, fast iteration

lots of existing specfiles for services today (k8s)

lots of tooling (docs generation, Postman, etc)

---
class: biglist

.left-column[
## Advantages of spec&#8209;first
## Separation of concerns
]

.right-column[
1. No HTTP primitives leak into code
1. Library models don't leak into your code
1. Business logic implemented in a trait enhances testability
]

???
status codes, content-type, encoding, serde

should be possible to switch libraries, akka -> pikko, http4s -> zio-http

---
class: biglist

.left-column[
## Advantages of spec&#8209;first
## Separation of concerns
## Human readable output
]
.right-column[
1. Comprehensive object models from specification
1. No exceptions, no nulls
1. Escape hatches in case of emergencies
]
???

Escape hatches:
- x-scala-type
- x-server-raw-response
- turn off the plugin and move the generated sources into your project

---
background-image: url("images/guardrail.jpg")
class: bg-cover

???
Presenting guardrail. A code generator that produces code you would have written yourself.

---
# `guardrail`

1\. Specification-first development

.spec-first-yaml[
```openapi
/login:
 post:
  operationId: login
  requestBody:
   required: true
   content:
    application/x-www-form-urlencoded:
     schema:
      required: [ username, password ]
      properties:
       username:
        type: string
       password:
        type: string
  responses:
   '200':
    content:
     application/json:
      schema:
       $ref: '#/components/schemas/LoginPreseed'
   '403':
    description: Bad login
```
]
.spec-first-code[
```scala
trait Handler {
  def login(
    respond: Resource.LoginResponse.type
  )(
    username: String,
    password: String
  ): scala.concurrent.Future[Resource.LoginResponse]
}
```
]
???
OpenAPI is a widely adopted standard, you may already have a specification solely for documentation purposes

---
# `guardrail`

2\. Strong separation of concerns
```scala
object GuardrailApp extends App {
  implicit val system = akka.actor.ActorSystem()

  val handler = new generated.Handler {
*   def login(respond: LoginResponse.type
            )(username: String,
              password: String
*           ): Future[LoginResponse] =
      (username, password) match {
        case ("functional", "scala") =>
*         Future.successful(respond.OK(...))
        case _ =>
*         Future.successful(respond.Forbidden)
      }
  }

  // Generate routing layer...
  val routes: akka.http.scaladsl.server.Route = generated.Resource.routes(handler)
  // or, just call your function:
  val plain: Future[LoginResponse] =
    handler.login(LoginResponse)("functional", "scala")
}
```
???
Business logic is described entirely by a trait and case classes...

---
# `guardrail`

2\. Strong separation of concerns
```scala
object GuardrailApp extends App {
  implicit val system = akka.actor.ActorSystem()

  val handler = new generated.Handler {
    def login(respond: LoginResponse.type
            )(username: String,
              password: String
            ): Future[LoginResponse] =
      (username, password) match {
        case ("functional", "scala") =>
          Future.successful(respond.OK(...))
        case _ =>
          Future.successful(respond.Forbidden)
      }
  }

  // Generate routing layer...
* val routes: akka.http.scaladsl.server.Route = generated.Resource.routes(handler)
  // or, just call your function:
* val plain: Future[LoginResponse] =
*   handler.login(LoginResponse)("functional", "scala")
}
```
???
... handler instance gets passed into a routing function to translate into your http library


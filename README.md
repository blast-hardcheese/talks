Steps:

1. Adding skeleton

  Use `new http4s/http4s.g8 -b 0.23` at the sbt console to set up a new project with sane defaults

2. Adding `sbt-revolver`

  Chuck in [`sbt-revolver`](https://github.com/spray/sbt-revolver) for ease of maintaining a forked JVM from inside sbt

3. Adding a basic OpenAPI specification and guardrail config

  Just a simple specification, the service can't fail, and doesn't return anything but an empty `200 OK`

4. Adding the only possible implementation for PetRoutes

  As the only thing we can do is return `200 OK`, just do that, as well as wiring it up into the routing layer.

  Test with `reStart` in SBT and `curl -v localhost:8080/pets` in your shell.

5. Deleting stub routes as we do not need them

  Now that we're wired up, we can delete the giter8-generated services (`HelloWorld` and `Jokes`) to reduce clutter

6. Adding a body to spec

  Extend our specification with a simple `application/json` body, a JSON-encoded String.

7. Extending listPets with Pets/Pet object

  Extend our specification with additional objects, `Pet` and `Pets` (where `Pets` is a simple array of `Pet`)

8. Filter static list of pets by tag

  Add a query parameter, permitting filtering the list of pets by one or more tags.

  ```bash
  curl 'localhost:8080/pets?tag=foo&tag=bitey'
  ```

9. Break out pets list for reuse

  Some refactoring, in preparation for more routes.

10. Adding getPet

  A simple route for fetching an individual pet by id, returning either 200 with the pet or 404 with a message

11. Adding http4s client

  Add the configuration for a guardrail-generated client in the `Test` scope

12. Adding a simple test for `listPets`

  Use our test client to write a functional test for the `listPets()` route

13. Adding a more complex test for `getPet`

  Use our test client to write a test that hits the backend multiple times.

14. Adding dadjoke client

  Add a new spec for [icanhazdadjoke.com](https://icanhazdadjoke.com) and generate a client for it.

15. Tell a joke if we can't find a pet

  Integrate that client with our Pets service, so if a pet can't be found at least you can hear a joke.

  ```bash
  $ curl 'http://localhost:8080/pet/5'
  {"message":"No pet found with id=5","joke":"I used to be addicted to the hokey pokey, but I turned myself around."}
  ```

16. Stubbing out joke client in tests

  With our new integration, the tests no longer compile. Add a static service that returns 404 for all requests.

17. Implement static Joke service

  Add an additional `Test`-scoped guardrail configuration to generate server definitions for the jokes API, so we can flesh out the tests.

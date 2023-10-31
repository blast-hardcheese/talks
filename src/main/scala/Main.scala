object App extends App {
  val json = JsonObject(Map(
    "a" -> JsonNumber(1),
    "b" -> JsonString("two"),
  ))

  println(JsonDecoder[Foo].fromJSON(json))
}

object App extends App {
  val json = JsonObject(Map(
    "a" -> JsonNumber(1),
    "b" -> JsonString("two"),
    "c" -> JsonNumber(3),
  ))

  println(JsonDecoder[Foo].fromJSON(json))
}

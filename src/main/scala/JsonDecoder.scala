import scala.util.Try

/* JsonDecoder
 *
 * A simple decoder for JsonNode.
 */
trait JsonDecoder[A] {
  def fromJSON(node: JsonNode): Option[A]
}

object JsonDecoder {
  def build[A](func: JsonNode => A): JsonDecoder[A] = new JsonDecoder[A] {
    def fromJSON(node: JsonNode) = Try(func(node)).toOption
  }

  implicit val decodeDouble: JsonDecoder[Double] =
    build({ case JsonNumber(value) => value })

  implicit val decodeInt: JsonDecoder[Int] =
    build({ case JsonNumber(value) => value.toInt })

  implicit val decodeLong: JsonDecoder[Long] =
    build({ case JsonNumber(value) => value.toLong })

  implicit val decodeString: JsonDecoder[String] =
    build({ case JsonString(value) => value })

  implicit def decodeArray[A: JsonDecoder]: JsonDecoder[List[A]] =
    new JsonDecoder[List[A]] {
      def fromJSON(node: JsonNode) = node match {
        case JsonArray(xs) =>
          val inner = implicitly[JsonDecoder[A]]
          xs.foldRight[Option[List[A]]](Some(List.empty)) { case (next, acc) =>
            acc.flatMap { outer => inner.fromJSON(next).map { x => x :: outer } }
          }
        case _ => None
      }
    }

  def apply[A: JsonDecoder]: JsonDecoder[A] = implicitly
}

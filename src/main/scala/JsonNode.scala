/* JsonNode
 *
 * A _very_ simple JSON representation
 */
trait JsonNode
case class JsonNumber(value: Double) extends JsonNode
case class JsonString(value: String) extends JsonNode
case class JsonArray(value: List[JsonNode]) extends JsonNode
case class JsonObject(value: Map[String, JsonNode]) extends JsonNode

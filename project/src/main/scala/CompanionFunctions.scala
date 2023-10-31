object CompanionFunctions {
  import scala.meta._

  def buildApply(typeName: Type.Name, parameters: List[Term.Param]): Defn.Def = {
    val assignedParameters = parameters.map { case param"${term@Term.Name(_)}: $_" =>
      q"$term = $term"
    }

    q"""
      def apply(..${parameters}): ${typeName} = new ${typeName}(..$assignedParameters)
    """
  }

  def buildDecoder(
    typeName: Type.Name,
    termName: Term.Name,
    parameters: List[Term.Param],
  ): Defn.Val = {
    val pairs: List[(Enumerator.Generator, Term.Assign)] = parameters.map {
      case param"${term@Term.Name(name)}: ${Some(tpe)}" =>
        val gen = Enumerator.Generator(
          // Bind the parameter name to...
          Pat.Var(term),
          // the result of decoding the value
          q"value.get(${Lit.String(name)}).flatMap(JsonDecoder[$tpe].fromJSON)"
        )

        (gen, q"$term = $term")
    }

    val body = if (pairs.length > 0) {
      val (generators, assignedParameters) = pairs.unzip
      Term.ForYield(generators, q"${termName}(..${assignedParameters})")
    } else q"Some(${termName}())"

    q"""
      implicit val decoder: JsonDecoder[${typeName}] = new JsonDecoder[${typeName}] {
        def fromJSON(node: JsonNode) = node match {
          case JsonObject(value) => ${body}
          case _                 => None
        }
      }
    """
  }
}

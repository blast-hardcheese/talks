object PojoFunctions {
  import _root_.scala.meta._

  // Build up the full AST for a useful toString implementation
  def buildToString(className: String, parameters: List[Term.Param], redactedFields: List[Term.Name]): Defn.Def = {
    val toStringTerm = {
      val fields: List[Term.ApplyInfix] = parameters.map {
        case param"${term@Term.Name(field)}: $_" =>
          val value = if (redactedFields.exists(_.value == term.value)) Lit.String("***") else q"${term}.toString()"
          q"${Lit.String(field + " = ")} + ${value}"
      }

      val commaSeparated = fields.foldLeft[Option[Term]](None) {
        case (None, term) => Some(term)
        case (Some(acc), term) => Some(q"$acc + ${Lit.String(", ")} + ${term}")
      }

      val start = Lit.String(className + "(")
      val end = q""" ")" """
      commaSeparated.fold(q"$start + $end") { inner =>
        q"$start + $inner + $end"
      }
    }

    q"""
      override def toString(): String = ${toStringTerm}
    """
  }

  def buildCopy(
    typeName: Type.Name,
    termName: Term.Name,
    parameters: List[Term.Param],
  ): Defn.Def = {
    // Easily translate
    //   param"a: Int"
    // into
    //   param"a: Int = this.a"
    val defaultParameters = parameters.map {
      case param"${term@Term.Name(_)}: $tpe" =>
        param"$term: $tpe = this.$term"
    }

    val assignedParameters = parameters.map {
      case param"${term@Term.Name(_)}: $_" =>
        q"$term = $term"
    }

    q"""
      def copy(..${defaultParameters}): ${typeName} = ${termName}(..${assignedParameters})
    """
  }
}

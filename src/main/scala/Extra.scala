object intp {
  def interpret(s: String) = ???
}

object ReplDefinitions {
  def javaFunc(x: Int, y: Long, z: String): String = null
  def validateThing(s: String): Option[Int] = Some(s.length).filterNot(_ == 0)

  case class User(email: String)

  def getUser(id: Long): User = null
  def validateEmail(email: String): String = null

  def genLift(i: Int): String = {
    val types: String = (1 to i).map(x => s"A${x}").mkString(", ")
    val ftypes: String = (1 to i).map(x => s"F[A${x}]").mkString(", ")
    val fargs: String = (1 to i).map(x => s"fa${x}").mkString(", ")
    val calls: String = (1 to i).foldLeft("wrap(f.curried)") { case (a, x) => s"call(fa${x})(${a})" }

    s"""
    |implicit class Lift6Applicative[F[_]](context: Applicative[F]) {
    |  import context.{ wrap, call }
    |
    |  def lift${i}[${types}, Result](f: ($types) => Result): (${ftypes}) => F[Result] = {
    |    case (${fargs}) =>
    |      ${calls}
    |  }
    |}""".stripMargin
  }

  def buildLift(i: Int): Unit = {
    val s = genLift(i)
    println(s)
    intp.interpret(s)
  }
}

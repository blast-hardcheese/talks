object Presentation {

Option(javaFunc(1, 2L, "Foo")
  ).flatMap(validateThing)


val someUser = getUser(12345L)
var result: String = null
if (someUser != null) {
    result = validateSomeEmail(someUser.email)
}

if (result == null)
  throw new Exception("Something went wrong!")


trait Tagger[T] {
  sealed trait Internal
  type Type = T with Internal

  def apply(value: T): Type = value.asInstanceOf[Type]
  def unapply(value: Type): Option[T] = Some(value)
}

object AssetId extends Tagger[Long]
type AssetId = AssetId.Type


// Pack
val id = AssetId(12345L)

// Unpack
val AssetId(_id) = id


object FirstName extends Tagger[String]
type FirstName = FirstName.Type

object LastName extends Tagger[String]
type LastName = LastName.Type

object FormattedName extends Tagger[String]
type FormattedName = FormattedName.Type


val fn = FirstName("Thomas")
val ln = LastName("Anderson")

// (FirstName, LastName) => String
val formatName: (FirstName, LastName) => FormattedName = {
  case (FirstName(first), LastName(last)) =>
    FormattedName(s"${first} ${last}")
}


formatName(fn, ln)


formatName(ln, fn)


val fn2: String = "Thomas"
val ln2: String = "Anderson"

def formatName(first: String, last: String): String = {
  s"${first} ${last}"
}

formatName(fn2, ln2)

// Works! Bonus!
formatName(ln2, fn2)


  def formatAddress(number: String, name: String): String =
    formatName(number, name)


type AnnouncementId = Long
type UserId = Long
type Name = String

case class Image(url: String)

case class Announcement(id: AnnouncementId,
                        title: String,
                        thumbnail: Image,
                        text: String)

case class Account(id: UserId,
                name: Name,
                profileImage: Image)


@scala.annotation.implicitNotFound(
  "No type class Presentable in scope for ${T}")
trait Presentable[T] {
  def title(elem: T): String
  def displayImage(elem: T): Image
}

object Presentable {
  def title[T](elem: T
    )(implicit ev: Presentable[T]
    ) = ev.title(elem)

  def displayImage[T](elem: T
    )(implicit ev: Presentable[T]
    ) = ev.displayImage(elem)
}


implicit object PresentableAnnouncement extends Presentable[Announcement] {
  def title(elem: Announcement) = elem.title
  def displayImage(elem: Announcement) = elem.thumbnail
}

implicit object PresentableUser extends Presentable[Account] {
  def title(elem: Account) = elem.name
  def displayImage(elem: Account) = elem.profileImage
}


val user = Account(
  id=500L,
  name="testuser",
  profileImage=Image("http://example.com/500/profile.png"))

val userTitle = Presentable.title(user)
val userImage = Presentable.displayImage(user)


type PostId = Long
case class Post(id: PostId, title: String, thumbnail: Image)


implicit object PresentablePost extends Presentable[Post] {
  def title(elem: Post) = elem.title
  def displayImage(elem: Post) = elem.thumbnail
}


val post = Post(
  id=100L,
  title="Hello, world!",
  thumbnail=Image("http://example.com/100/thumb.png"))

val title = Presentable.title(post)
val image = Presentable.displayImage(post)


object Presentable2 {
  def title(elem: Any): String = elem match {
    case elem: Announcement => elem.title
    case elem: Account => elem.name
  }

  def displayImage(elem: Any): Image = elem match {
    case elem: Announcement => elem.thumbnail
    case elem: Account => elem.profileImage
  }
}


val userTitle2 = Presentable2.title(user)
val userImage2 = Presentable2.displayImage(user)


lazy val postTitle2 = Presentable2.title(post)


println(postTitle2)


sealed trait Validation[+Err, +A]
case class Success[A](value: A) extends Validation[Nothing, A]
case class Failure[E](value: E) extends Validation[E, Nothing]


type ValidOrErrorStrings[+A] = Validation[List[String], A]


def success[T](x: T): ValidOrErrorStrings[T] = Success(x)
def fail[T](err: String): ValidOrErrorStrings[T] = Failure(List(err))


trait Applicative[F[_]] {
  def wrap[A](value: A): F[A]
  def call[A, B](fa: => F[A])(ff: F[A => B]): F[B]

  def lift[A, Result](f: A => Result): F[A] => F[Result] = {
    case fa =>

    call(fa)(wrap(f))
  }
}


buildLift(6)


implicit object ValidationContext extends Applicative[ValidOrErrorStrings] {
  def wrap[A](value: A) = Success(value)
  def call[A, B](fa: => ValidOrErrorStrings[A])(ff: ValidOrErrorStrings[A => B]): ValidOrErrorStrings[B] = {
    (fa, ff) match {
      case (Success(a), Success(f)) => Success(f(a))
      case (Failure(a), Failure(f)) => Failure(f ++ a)
      case (Failure(a), _) => Failure(a)
      case (_, Failure(f)) => Failure(f)
    }
  }
}


def addTwo[F[_]](value: F[Int])(implicit ctx: Applicative[F]) = {
  ctx.call(value)(ctx.wrap((_: Int) + 2))
}


addTwo(success(2))


addTwo(fail[Int]("Something terrible happened!"))


object Username extends Tagger[String]
type Username = Username.Type

type RawUsername = String

val validateUsername: RawUsername => ValidOrErrorStrings[Username] = {
  case "" => fail("Username must not be empty!")

  case x if Set("foo", "bar", "baz").contains(x) =>
    fail(s"${x} is not a valid username!")

  case x if x.length > 16 => fail("Username is too long!")

  case valid => success(Username(valid))
}


import uk.gov.hmrc.emailaddress.EmailAddress
type RawEmail = String
object Email extends Tagger[String]
type Email = Email.Type

val validateEmail: RawEmail => ValidOrErrorStrings[Email] = {
  case "" => fail("Email must not be empty!")

  case x if x.endsWith("@example.com") =>
    fail("Please use a valid domain!")

  case x if !EmailAddress.isValid(x) => fail("Invalid email!")

  case valid => success(Email(valid))
}


type RawAge = Int
object Age extends Tagger[Int]
type Age = Age.Type

val validateAge: RawAge => ValidOrErrorStrings[Age] = {
  case x if x < 0 => fail("Invalid age!")
  case valid => success(Age(valid))
}


//-Defined in previous slides
type RawFirstName = String
//+object FirstName extends Tagger[String]
//+type FirstName = FirstName.Type

type RawLastName = String
//+object LastName extends Tagger[String]
//+type LastName = LastName.Type

def validateString[T](wrap: String => T
    ): String => ValidOrErrorStrings[T] = {
  case "" => fail("Must not be empty!")
  case x if x.length > 16 => fail("Maximum length of 16 characters")
  case valid => success(wrap(valid))
}

val validateFirstName: RawFirstName => ValidOrErrorStrings[FirstName] =
  validateString(FirstName(_))
val validateLastName: RawLastName => ValidOrErrorStrings[LastName] =
  validateString(LastName(_))


case class User(id: Long, username: Username, email: Email,
    age: Age, firstName: FirstName, lastName: LastName)

def createUser(id: Long, username: RawUsername, email: RawEmail,
    age: RawAge, firstName: RawFirstName, lastName: RawLastName
    ): ValidOrErrorStrings[User] = {
  val ValidUser = ValidationContext.lift6(User)

  ValidUser(
    success(id),
    validateUsername(username),
    validateEmail(email),
    validateAge(age),
    validateFirstName(firstName),
    validateLastName(lastName)
  )
}


val validUser = createUser(123L, "neo", "tanderson@metacortex.com",
    32, "Thomas", "Anderson")


val invalidUser = createUser(123L, "foo", "bar@example.com",
    -100, "Somebody", "")


case class User2(id: Long, username: String, email: String,
    age: Int, firstName: String, lastName: String)


val validateUsername2: String => Either[String, String] = {
  case "" => Left("Username must not be empty!")
  case x if Set("foo", "bar", "baz").contains(x) => Left("${x} is not a valid username!")
  case x if x.length > 16 => Left("Username is too long!")
  case valid => Right(valid)
}


val validateEmail2: String => Either[String, String] = {
  case "" => Left("Email must not be empty!")
  case x if x.endsWith("@example.com") => Left("Please use a valid domain!")
  case x if !EmailAddress.isValid(x) => Left("Invalid email!")
  case valid => Right(valid)
}


val validateAge2: Int => Either[String, Int] = {
  case x if x < 0 => Left("Invalid age!")
  case valid => Right(valid)
}

val validateName: String => Either[String, String] = {
  case "" => Left("Must not be empty!")
  case x if x.length > 16 => Left("Maximum length of 16 characters")
  case valid => Right(valid)
}


def createUser_(id: Long, username: String, email: String,
    age: Int, firstName: String, lastName: String
    ): Either[List[String], User2] = ???


def createUser2(id: Long, username: String, email: String,
    age: Int, firstName: String, lastName: String
    ): Either[List[String], User2] = {

  val _username = validateUsername2(username)
  val _email = validateEmail2(email)
  val _age = validateAge2(age)
  val _firstName = validateName(firstName)
  val _lastName = validateName(lastName)

  val errors: List[String] = List(
    _username.left.toOption,
    _email.left.toOption,
    _age.left.toOption,
    _firstName.left.toOption,
    _lastName.left.toOption
  ).flatten

  if (errors.isEmpty) {
    Right(User2(id, _username.right.get, _email.right.get,
      _age.right.get, _firstName.right.get, _lastName.right.get))
  } else {
    Left(errors)
  }
}


val validUser2 = createUser2(123L, "neo", "tanderson@metacortex.com",
    32, "Thomas", "Anderson")


val invalidUser2 = createUser2(123L, "foo", "bar@example.com",
    -100, "Somebody", "")


    implicit class Lift6Applicative[F[_]](context: Applicative[F]) {
        import context.{ wrap, call }

        def lift6[A1, A2, A3, A4, A5, A6, Result](f: (A1, A2, A3, A4, A5, A6) => Result): (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6]) => F[Result] = {
            case (fa1, fa2, fa3, fa4, fa5, fa6) =>
                call(fa6)(call(fa5)(call(fa4)(call(fa3)(call(fa2)(call(fa1)(wrap(f.curried)))))))
        }
    }

  def javaFunc(x: Int, y: Long, z: String): String = null
  def validateThing(s: String): Option[Int] = Some(s.length).filterNot(_ == 0)

  case class JUser(email: String)

  def getUser(id: Long): JUser = null
  def validateSomeEmail(email: String): String = null

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
  }
}

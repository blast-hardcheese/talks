name := "fp-is-overrated"

scalaVersion := "2.11.8"

resolvers += Resolver.bintrayRepo("hmrc", "releases")

libraryDependencies += "uk.gov.hmrc" %% "emailaddress" % "1.1.0"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.2"

scalacOptions ++= Seq("-feature", "-language:higherKinds")

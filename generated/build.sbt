val akkaHttpVersion        = "10.2.10"
val akkaVersion            = "2.6.20"
val catsVersion            = "2.10.0"
val circeVersion           = "0.14.6"
val javaxAnnotationVersion = "1.3.2"
val jaxbApiVersion         = "2.3.1"

libraryDependencies ++= Seq(
  "javax.annotation"  %  "javax.annotation-api" % javaxAnnotationVersion, // for jdk11
  "javax.xml.bind"    %  "jaxb-api"             % jaxbApiVersion, // for jdk11

  "com.typesafe.akka" %% "akka-http"            % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-testkit"    % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream"          % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit"         % akkaVersion,
  "io.circe"          %% "circe-core"           % circeVersion,
  "io.circe"          %% "circe-jawn"           % circeVersion,
  "io.circe"          %% "circe-parser"         % circeVersion,
  "org.typelevel"     %% "cats-core"            % catsVersion
)

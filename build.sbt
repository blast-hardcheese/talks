val Http4sVersion = "0.23.10"
val CirceVersion = "0.14.1"
val MunitVersion = "0.7.29"
val LogbackVersion = "1.2.10"
val MunitCatsEffectVersion = "1.0.7"

lazy val root = (project in file("."))
  .settings(
    organization := "dev.guardrail.talks",
    name := "guardrail-talk",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.6",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-ember-server" % Http4sVersion,
      "org.http4s"      %% "http4s-ember-client" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "io.circe"        %% "circe-generic"       % CirceVersion,
      "org.scalameta"   %% "munit"               % MunitVersion           % Test,
      "org.typelevel"   %% "munit-cats-effect-3" % MunitCatsEffectVersion % Test,
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion         % Runtime,
      "org.scalameta"   %% "svm-subs"            % "20.2.0"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.13.2" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
    testFrameworks += new TestFramework("munit.Framework"),

    // guardrail internals currently import too many things and define parameters that are not always used.
    // If this is a problem, defining a subproject explicitly to contain your
    // generated code and using `dependOn` to bring that into your project lets you scope your warnings.
    scalacOptions --= Seq("-Wunused:imports", "-Wunused:params"),

    // A single server, based on `myserver.yaml`, using http4s
    Compile / guardrailTasks += ScalaServer(file("myserver.yaml"), "dev.guardrail.example.routes", framework="http4s")
  )

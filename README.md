Functional Programming is Overrated
===================================

A tongue-in-cheek exercise in refactoring a functionally designed, generally decoupled REST service (built with akka-http and doobie) into a tightly coupled, less typesafe REST service with unconstrained effects.

Talking points:

 - Primitive type tagging (Inspired by [shapeless](https://github.com/milessabin/shapeless/blob/shapeless-2.3.1/core/src/main/scala/shapeless/typeoperators.scala#L25-L34), [scalaz](https://github.com/scalaz/scalaz/blob/v7.2.2/core/src/main/scala/scalaz/package.scala#L99-L110), further reading: [refined](https://github.com/fthomas/refined), [bond](https://github.com/fwbrasil/bond))
 - Typeclasses
 - Lenses (of the [shapeless](https://github.com/milessabin/shapeless/blob/shapeless-2.3.1/core/src/main/scala/shapeless/lenses.scala) variety)
 - Validation (from [Scalaz](https://github.com/scalaz/scalaz/blob/v7.2.2/core/src/main/scala/scalaz/Validation.scala))

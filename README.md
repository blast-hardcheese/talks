swagger-codegen Overview and Usage
==================================


Talking points:
  - Problem
    - Large networked systems require lots of REST/RPC APIs
    - Writing clients and servers by hand is time-consuming
    - Framework migrations require effort, if even possible
    - Clients in incompatible languages must be written from scratch
  - Requirements
    - Our API surface area must be written in a machine-readable way using existing standards
    - From that specification file, a developer must be able to automatically generate working clients and servers
    - These clients and servers must not expose the implementation details of the protocol
    - These clients and servers must not compile if the contract is violated
  - Solution
    - Specification file: Swagger
    - Code: scala.meta-generated akka-http + cats + circe
    - Business logic passed into routing layer as a parameter, clients look like normal async calls after construction
    - As clients and servers don't expose the underlying protocol, there is no way to violate the specification

Following along:

```
computer user$ sbt console
[info] Loading global plugins from /home/user/.sbt/0.13/plugins
...
scala> :load present.txt
```

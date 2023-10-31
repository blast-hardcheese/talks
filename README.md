Bespoke code generation
===

Blurb:
  Code generation facilitating separation between application domain and networking domain

Premise:
  One of the critical markers for success of a new project is how quickly you can validate assumptions.
  Tight coupling between business logic and tooling can increase speed at the cost of technical debt.
  Can we use bespoke SBT plugins in order to both automate boilerplate and make standarization easier?

Goals:
  1. Write a simple POJO generator in scalameta
  2. Extend the POJO generator to decode JSON
  3. Add field redaction for logging

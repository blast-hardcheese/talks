# Automate more code with scalameta

This was an unconference workshop, building up intuitive knowledge of scalameta at the Scala REPL, finalizing with something generated that looks like a typeclass.

The full REPL log can be found [here](repl.log)!

### Playing around building static methods on an object

```
val params = (1 until 10).toList.map { i =>
  val params: List[Term.Param] = (1 to i).toList.map { j =>
    Term.Param(Nil, Term.Name(s"a${j}"), Some(Type.Name("Int")), None)
  }
  q"implicit def ${Term.Name(s"foo${i}")}(..${params}) = ${Lit.Int(i)}"
}

q"object Foo { ..${params} }"
```

```
implicitly[ShowTuple[Int]].show(Tuple1(5))
res50: String = 5
```

### Inventing something typeclass-y for all sizes of Tuple
```
trait ShowTuple[A1] { def show(vals: Tuple1[A1]): String }

implicit object ShowTupleInt extends ShowTuple[Int] {
  def show(vals: Tuple1[Int]): String = { val Tuple1(a) = vals; a.toString }
}
```

### Creating boilerplate for something typeclass-y for all sizes of Tuple
```
(1 until 22).foreach { size =>
  val (tparams, tparams1) = (1 to size).toList.map { i =>
  val tname = Type.Name(s"A${i}")
    (Type.Param(Nil, tname, Nil, Type.Bounds(None, None), Nil, Nil),
      tname)
  }.unzip
  val defn = q"trait ${Type.Name(s"ShowTuple${size}")}[..${tparams}] { def show(vals: ${Type.Name(s"Tuple${size}")}[..${tparams1}]): String }"
  println(defn)
}
```

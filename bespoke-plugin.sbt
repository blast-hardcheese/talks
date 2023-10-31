import _root_.sbt.Keys._
import _root_.sbt.util.CacheStore
import _root_.sbt.util.Cache
import _root_.sbt.util.CacheImplicits._

// The entrypoint of our project's source code generator
def emitSources(sourceManagedPath: java.io.File): List[(scala.meta.Source, java.io.File)] = {
  import _root_.scala.meta._
  List(
    (buildPOJO("Foo", List(param"a: Int", param"b: String")), sourceManagedPath / "Foo.scala"),
    (buildPOJO("Bar", List(param"a: Int", param"b: Int", param"c: Int")), sourceManagedPath / "Bar.scala"),
    (buildPOJO("Baz", List.empty), sourceManagedPath / "Baz.scala"),
  )
}

def buildPOJO(className: String, parameters: List[scala.meta.Term.Param]): scala.meta.Source = {
  import _root_.scala.meta._

  // Since the same token (eg: Foo) can occur in both the
  // value position (object Foo, Foo(...)) as well as the
  // type position (new Foo(...), def ...: Foo = ...,
  // we need both Term and Type names:
  val termName = Term.Name(className)
  val typeName = Type.Name(className)

  val accessorParams = parameters.map { case param"$term: $tpe" => 
    param"val $term: $tpe"
  }

  source"""
    object ${termName} {
      ${CompanionFunctions.buildApply(typeName, parameters)};
      ${CompanionFunctions.buildDecoder(typeName, termName, parameters)};
    }

    class ${typeName}(..${accessorParams}) {
      ${PojoFunctions.buildCopy(typeName, termName, parameters)};
      ${PojoFunctions.buildToString(className, parameters)};
    }
  """
}

// A utility function to write scalameta ASTs into files
def writeFiles(outputs: List[(scala.meta.Source, java.io.File)]): List[java.io.File] = {
  import java.nio.file.Files
  import java.nio.file.Path
  import java.io.FileOutputStream
  outputs.map { case (src, dest) =>
    Files.createDirectories(Path.of(dest.getParent()))
    val fos = new FileOutputStream(dest)
    fos.write(src.syntax.getBytes())
    fos.close()
    dest
  }
}

lazy val generateTask = taskKey[List[java.io.File]]("A source-code-generating task")
generateTask := {
  // See SBT documentation on Caching to avoid triggering a recompilation:
  // https://www.scala-sbt.org/1.x/docs/Caching.html#Caching
  val sourceManagedPath = (Compile / sourceManaged).value
  writeFiles(emitSources(sourceManagedPath))
}

Compile / sourceGenerators += generateTask

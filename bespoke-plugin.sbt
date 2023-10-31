import _root_.sbt.Keys._
import _root_.sbt.util.CacheImplicits._

// The entrypoint of our project's source code generator
def emitSources(sourceManagedPath: java.io.File): List[(scala.meta.Source, java.io.File)] = {
  import _root_.scala.meta._
  List.empty
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

package scala.tools.apiSearch.featureExtraction

import java.io.File
import java.util.jar.JarFile

import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.apiSearch.model.Entity
import scala.tools.nsc.interactive.Global

class JarExtractor(val compiler: Global) {
  val scalaExtractor = new ScalaSourceExtractor(compiler)

  def apply(file: File): Stream[Entity] = {
    val jar = new JarFile(file)

    println(jar)

    jar.entries().toStream.flatMap { entry =>
      if (!entry.isDirectory && entry.getName.endsWith(".scala")) {
        println(entry.getName)
        val source = scala.io.Source.fromInputStream(jar.getInputStream(entry)).toSeq
        val sourceFile = new BatchSourceFile(entry.getName, source)
        scalaExtractor(sourceFile)
      } else {
        Stream.empty
      }
    }
  }
}

package scala.tools.apiSearch.featureExtraction

import scala.collection.JavaConversions._
import scala.tools.apiSearch.model._
import scala.tools.nsc.interactive.Global
import java.util.jar.JarFile
import java.io.InputStreamReader
import scala.collection.mutable.ArrayBuilder
import scala.reflect.internal.util.BatchSourceFile
import java.io.File

class JarExtractor(val compiler: Global) {
  val scalaExtractor = new ScalaSourceExtractor(compiler)

  def apply(file: File): Stream[Entity] = {
    val jar = new JarFile(file)

    jar.entries().toStream.flatMap { entry =>
      if (!entry.isDirectory && entry.getName.endsWith(".scala")) {
        val source = scala.io.Source.fromInputStream(jar.getInputStream(entry)).toSeq
        val sourceFile = new BatchSourceFile(entry.getName, source)
        scalaExtractor(sourceFile)
      } else {
        Stream.empty
      }
    }
  }
}

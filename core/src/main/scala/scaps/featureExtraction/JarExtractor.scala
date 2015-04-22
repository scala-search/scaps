package scaps.featureExtraction

import java.io.File
import java.util.jar.JarFile
import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.reflect.internal.util.BatchSourceFile
import scaps.model.Entity
import scala.tools.nsc.interactive.Global
import scala.io.Codec

class JarExtractor(val compiler: Global) {
  val scalaExtractor = new ScalaSourceExtractor(compiler)

  def apply(file: File): Stream[Entity] = {
    val jar = new JarFile(file)

    jar.entries().toStream.flatMap { entry =>
      if (!entry.isDirectory && entry.getName.endsWith(".scala")) {
        val source = scala.io.Source.fromInputStream(jar.getInputStream(entry))(Codec.UTF8).toSeq
        val sourceFile = new BatchSourceFile(entry.getName, source)
        scalaExtractor(sourceFile)
      } else {
        Stream.empty
      }
    }
  }
}

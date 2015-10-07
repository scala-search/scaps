package scaps.scala.featureExtraction

import java.io.File
import java.util.jar.JarFile

import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.reflect.internal.util.BatchSourceFile
import scaps.api.Definition
import scala.tools.nsc.doc.ScaladocGlobal
import scala.io.Codec
import scalaz._

class JarExtractor(val compiler: ScaladocGlobal) {
  val scalaExtractor = new ScalaSourceExtractor(compiler)

  def apply(file: File): List[ExtractionError \/ Definition] = {
    val jar = new JarFile(file)

    val files = jar.entries().toList.flatMap { entry =>
      if (!entry.isDirectory && entry.getName.endsWith(".scala")) {
        val source = scala.io.Source.fromInputStream(jar.getInputStream(entry))(Codec.UTF8).toSeq
        Some(new BatchSourceFile(entry.getName, source))
      } else {
        None
      }
    }

    scalaExtractor(files)
  }
}

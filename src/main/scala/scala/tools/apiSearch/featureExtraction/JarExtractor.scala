package scala.tools.apiSearch.featureExtraction

import scala.collection.JavaConversions._
import scala.tools.apiSearch.model._
import scala.tools.nsc.interactive.Global
import java.util.jar.JarFile
import java.io.InputStreamReader
import scala.collection.mutable.ArrayBuilder
import scala.reflect.internal.util.BatchSourceFile
import rx.lang.scala.Observable

class JarExtractor(val compiler: Global) {
  val scalaExtractor = new ScalaSourceExtractor(compiler)

  def apply(path: String): (Observable[TemplateEntity], Observable[TermEntity]) = {
    val jar = new JarFile(path)

    val res = Observable.from(jar.entries().toStream).map { entry =>
      if (!entry.isDirectory && entry.getName.endsWith(".scala")) {
        val source = scala.io.Source.fromInputStream(jar.getInputStream(entry)).toSeq
        val sourceFile = new BatchSourceFile(entry.getName, source)
        scalaExtractor(sourceFile)
      } else {
        (Observable.empty, Observable.empty)
      }
    }

    (res.flatMap(_._1), res.flatMap(_._2))
  }
}

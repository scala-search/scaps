package scala.tools.apiSearch.benchmark

import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.tools.apiSearch.utils.CompilerAccess
import org.apache.lucene.store.Directory
import org.apache.lucene.store.FSDirectory
import java.nio.file.Path
import java.io.File
import java.nio.file.Paths
import scala.tools.apiSearch.index.TermsIndex
import scala.tools.apiSearch.index.ClassIndex
import scala.tools.apiSearch.model._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.tools.apiSearch.searching.QueryParser
import scala.tools.apiSearch.searching.Suggestion

object Benchmark extends App with CompilerAccess {
  val libraryPath = "/Applications/eclipseScala/plugins/org.scala-lang.scala-library.source_2.11.5.v20150101-184742-3fafbc204f.jar"
  val indexDir = "benchmark/target/index"

  val queries = Map[String, List[TermEntity => Boolean]](
    "Function4[List[String], String, String, String, String]" ->
      List(_.name.endsWith("#mkString")))

  val extractor = new JarExtractor(compiler)

  val termsIndex = {
    val termsDir = FSDirectory.open(Paths.get(indexDir, "terms").toFile())
    new TermsIndex(termsDir)
  }

  val classesIndex = {
    val classesDir = FSDirectory.open(Paths.get(indexDir, "classes").toFile())
    new ClassIndex(classesDir)
  }

  termsIndex.delete()
  classesIndex.delete()

  {
    val entities = extractor(new File(libraryPath))
    val f = Future { termsIndex.addEntities(entities.collect { case t: TermEntity => t }) }
    classesIndex.addEntities(entities.collect { case c: ClassEntity => c })
    Await.result(f, 1.hour)
  }

  val analyzer = new QueryAnalyzer(classesIndex.findClass _, classesIndex.findSubClasses _)

  queries.foreach {
    case (query, checkers) =>
      val raw = QueryParser(query).right.get

      val analyzed = analyzer(raw).get.right.get

      termsIndex.find(analyzed).get.foreach { t =>
        println(t)
        println(checkers.exists(_(t)))
      }
  }
}

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
import scala.tools.apiSearch.index.Indexer
import scala.tools.apiSearch.model._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.tools.apiSearch.searching.QueryParser
import java.util.Calendar
import java.text.SimpleDateFormat
import java.io.FileWriter
import scala.tools.apiSearch.utils.using
import scala.tools.apiSearch.settings.Settings
import scala.tools.apiSearch.featureExtraction.StandaloneExtractor

object Benchmark extends App {
  val outputDir = "benchmark/target/results"

  val settings = Settings.fromApplicationConf()

  val outputPath = {
    val output = new File(outputDir)
    output.mkdirs()
    val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val now = Calendar.getInstance.getTime
    s"$outputDir/${format.format(now)}.csv"
  }

  val rebuildIndex = true

  val queries = List(
    // magic integer values
    "Int",
    // instance of Ordering
    "Ordering[Char]",
    // how to create a range?
    "(Int, Int) => Range",
    // can we create a range from chars? (implicit conversions)
    "(Char, Char) => Range",
    // mkString (concrete query, simple generic result)
    "(List[String], String, String, String) => String",
    // filter (concrete query, generic result)
    "(List[Int], Function1[Int, Boolean]) => List[Int]",
    // reduce left (concrete query, highly generic result)
    "(List[Char], (Double, Char) => Double) => Double",
    // functions that split a list into two (generic query)
    "List[A] => (List[A], List[A])",
    // min/max (generic query with context bound)
    "(List[A], Ordering[A]) => A",
    // Future.sequence (highly generic with higher kinded type param)
    "(collection.Seq[concurrent.Future[A]]) => concurrent.Future[collection.Seq[A]]")

  val indexer = new Indexer(settings.index)

  if (rebuildIndex) {
    indexer.reset().get

    val entities = StandaloneExtractor(settings.extractor)

    Await.result(indexer.index(entities), 1.hour)
  }

  val analyzer = QueryAnalyzer(settings.query, indexer.classesIndex)

  using(new FileWriter(outputPath)) { writer =>
    writer.write("Query; Index; Result; Fingerprint;\n")

    queries.foreach { query =>
      println(query)

      val raw = QueryParser(query).right.get

      val analyzed = analyzer(raw).get.getOrElse(???)

      indexer.termsIndex.find(analyzed).get.take(20).zipWithIndex.foreach {
        case (t, idx) =>
          val term = t.withoutComment
          val entry = s"${query}; $idx; $term; ${term.fingerprint}\n"
          println(entry)
          writer.write(entry)
      }
    }
  }.get
}

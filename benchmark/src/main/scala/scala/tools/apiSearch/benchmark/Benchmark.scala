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
import scala.tools.apiSearch.searching.Suggestion
import java.util.Calendar
import java.text.SimpleDateFormat
import java.io.FileWriter
import scala.tools.apiSearch.utils.using

object Benchmark extends App with CompilerAccess {
  val libraryPath = "/Applications/eclipseScala/plugins/org.scala-lang.scala-library.source_2.11.5.v20150101-184742-3fafbc204f.jar"
  val indexDir = "benchmark/target/index"

  val outputPath = {
    val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val now = Calendar.getInstance.getTime
    s"benchmark/target/results/${format.format(now)}.csv"
  }

  val rebuildIndex = false

  val queries = List(
    // magic integer values
    "Int",
    // instance of Ordering
    "Ordering[Char]",
    // how to create a range?
    "Function2[Int, Int, Range]",
    // can we create a range from chars? (implicit conversions)
    "Function2[Char, Char, Range]",
    // mkString (concrete query, simple generic result)
    "Function4[List[String], String, String, String, String]",
    // filter (concrete query, generic result)
    "Function2[List[Int], Function1[Int, Boolean], List[Int]]",
    // reduce left (concrete query, highly generic result)
    "Function2[List[Char], Function2[Double, Char, Double], Double]",
    // functions that split a list into two (generic query)
    "Function2[List[A], Tuple2[List[A], List[A]]]",
    // min/max (generic query with context bound)
    "Function1[List[A], Ordering[A], A]",
    // Future.sequence (highly generic with higher kinded type param)
    "Function1[collection.Seq[Future[A], Future[collection.Seq[A]]]]")

  val indexer = new Indexer(indexDir)

  if (rebuildIndex) {
    indexer.reset().get

    val extractor = new JarExtractor(compiler)
    val entities = extractor(new File(libraryPath))

    Await.result(indexer.index(entities), 1.hour)
  }

  val analyzer = QueryAnalyzer(indexer.classesIndex)

  using(new FileWriter(outputPath)) { writer =>
    writer.write("Query; Index; Result;\n")

    queries.foreach { query =>
      val raw = QueryParser(query).right.get

      val analyzed = analyzer(raw).get.right.get

      indexer.termsIndex.find(analyzed).get.take(20).zipWithIndex.foreach {
        case (t, idx) =>
          val term = t.withoutComment
          val entry = s"${query}; $idx; $term;\n"
          println(entry)
          writer.write(entry)
      }
    }
  }.get
}

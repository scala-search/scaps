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

  val termsIndex = {
    val termsDir = FSDirectory.open(Paths.get(indexDir, "terms").toFile())
    new TermsIndex(termsDir)
  }

  val classesIndex = {
    val classesDir = FSDirectory.open(Paths.get(indexDir, "classes").toFile())
    new ClassIndex(classesDir)
  }

  if (rebuildIndex) {
    termsIndex.delete()
    classesIndex.delete()

    {
      val extractor = new JarExtractor(compiler)
      val entities = extractor(new File(libraryPath))
      val f = Future { termsIndex.addEntities(entities.collect { case t: TermEntity => t }) }
      classesIndex.addEntities(entities.collect { case c: ClassEntity => c })
      Await.result(f, 1.hour)
    }
  }

  val analyzer = new QueryAnalyzer(classesIndex.findClass _, classesIndex.findSubClasses _)

  using(new FileWriter(outputPath)) { writer =>
    writer.write("Query; Index; Result;\n")

    queries.foreach { query =>
      val raw = QueryParser(query).right.get

      val analyzed = analyzer(raw).get.right.get

      termsIndex.find(analyzed).get.take(20).zipWithIndex.foreach {
        case (t, idx) =>
          val term = t.withoutComment
          val entry = s"${query}; $idx; $term;\n"
          println(entry)
          writer.write(entry)
      }
    }
  }.get
}

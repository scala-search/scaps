package scala.tools.apiSearch.benchmark

import java.io.File
import java.io.FileWriter
import java.nio.file.Files
import java.nio.file.Path
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.tools.apiSearch.index.ClassIndex
import scala.tools.apiSearch.index.Indexer
import scala.tools.apiSearch.index.TermsIndex
import scala.tools.apiSearch.model._
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.tools.apiSearch.searching.QueryParser
import scala.tools.apiSearch.settings.Settings
import scala.tools.apiSearch.utils.CompilerAccess
import scala.tools.apiSearch.utils.using
import scala.collection.JavaConverters._

object Benchmark extends App with CompilerAccess {
  val outputDir = "benchmark/target/results"

  val settings = Settings.fromApplicationConf()
  val validationSettings = ValidationSettings.fromApplicationConf()

  val classPaths = for {
    project <- validationSettings.projects
    dependency <- project.dependencies
  } yield {
    val name = dependency.getPath.split("/").last
    val file = new File(validationSettings.downloadDir, name)

    if (!file.exists()) {
      import sys.process._
      (dependency #> file).!!
    }

    file.getAbsolutePath()
  }

  val compiler = initCompiler(classPaths)
  val extractor = new JarExtractor(compiler)

  val outputPath = {
    val output = new File(outputDir)
    output.mkdirs()
    val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val now = Calendar.getInstance.getTime
    s"$outputDir/${format.format(now)}.csv"
  }

  if (!validationSettings.downloadDir.exists()) {
    validationSettings.downloadDir.mkdirs()
  }

  val rebuildIndex = true

  val indexer = new Indexer(settings.index)

  if (rebuildIndex) {
    indexer.reset().get

    validationSettings.projects.foreach { project =>
      val jar = new File(validationSettings.downloadDir, project.name)
      println(jar)
      if (!jar.exists()) {
        import sys.process._
        (project.url #> jar).!!
      }

      val entities = extractor(jar)

      Await.result(indexer.index(entities), 1.hour)
    }
  }

  val analyzer = QueryAnalyzer(settings.query, indexer.classesIndex)

  using(new FileWriter(outputPath)) { writer =>
    writer.write("Query; Index; Result; Fingerprint;\n")

    validationSettings.queries.foreach {
      case (query, expectedResults) =>
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

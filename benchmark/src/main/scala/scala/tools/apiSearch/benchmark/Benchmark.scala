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
import scala.tools.apiSearch.SearchEngine
import scala.tools.apiSearch.index.TermsIndex
import scala.tools.apiSearch.model._
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.tools.apiSearch.searching.QueryParser
import scala.tools.apiSearch.settings.Settings
import scala.tools.apiSearch.featureExtraction.CompilerUtils
import scala.tools.apiSearch.utils.using
import scala.collection.JavaConverters._

object Benchmark extends App {
  val outputDir = "benchmark/target/results"

  val settings = Settings.fromApplicationConf
  val validationSettings = ValidationSettings.fromApplicationConf

  val engine = new SearchEngine(settings)

  val outputPath = {
    val output = new File(outputDir)
    output.mkdirs()
    val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val now = Calendar.getInstance.getTime
    s"$outputDir/${format.format(now)}.csv"
  }

  if (validationSettings.rebuildIndex) {
    val classPaths = initEnvironment(validationSettings)
    val compiler = CompilerUtils.initCompiler(classPaths)
    val extractor = new JarExtractor(compiler)

    engine.reset().get

    validationSettings.projects.foreach { project =>
      val jar = new File(validationSettings.downloadDir, project.name)
      println(jar)
      if (!jar.exists()) {
        import sys.process._
        (project.url #> jar).!!
      }

      Await.result(engine.indexEntities(extractor(jar)), 1.hour)
    }
  }

  using(new FileWriter(outputPath)) { writer =>
    writer.write("Query; Index; Result; Fingerprint;\n")

    validationSettings.queries.foreach {
      case (query, expectedResults) =>
        println(query)

        engine.search(query).get.fold(
          errors => println(errors),
          results => results.take(20).zipWithIndex.foreach {
            case (t, idx) =>
              val term = t.withoutComment
              val entry = s"${query}; $idx; $term; ${term.fingerprint}\n"
              println(entry)
              writer.write(entry)
          })
    }
  }.get

  def initEnvironment(settings: ValidationSettings) = {
    settings.downloadDir.mkdirs()

    for {
      project <- settings.projects
      dependency <- project.dependencies
    } yield {
      val file = new File(settings.downloadDir, dependency.name)

      if (!file.exists()) {
        import sys.process._
        (dependency.url #> file).!!
      }

      file.getAbsolutePath()
    }
  }
}

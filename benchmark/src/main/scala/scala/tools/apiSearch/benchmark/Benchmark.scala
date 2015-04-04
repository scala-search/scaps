package scala.tools.apiSearch.benchmark

import java.io.File
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.sys.process.urlToProcess
import scala.tools.apiSearch.featureExtraction.CompilerUtils
import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.tools.apiSearch.model.TermEntity
import scala.tools.apiSearch.searchEngine.SearchEngine
import scala.tools.apiSearch.settings.Settings
import scala.tools.apiSearch.utils.using

object Benchmark extends App {
  val outputDir = "benchmark/target/results"

  val settings = Settings.fromApplicationConf
  val validationSettings = ValidationSettings.fromApplicationConf

  val engine = SearchEngine(settings).get

  initEnvironment(validationSettings, engine)

  val outputPath = {
    val output = new File(outputDir)
    output.mkdirs()
    val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val now = Calendar.getInstance.getTime
    s"$outputDir/${format.format(now)}.csv"
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

  def avaragePrecision(expected: Seq[String], results: Seq[TermEntity]): Double = ???

  def initEnvironment(settings: ValidationSettings, engine: SearchEngine) = {
    if (settings.rebuildIndex) {
      settings.downloadDir.mkdirs()

      val classPaths = for {
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

      val compiler = CompilerUtils.initCompiler(classPaths)
      val extractor = new JarExtractor(compiler)

      engine.reset().get

      settings.projects.foreach { project =>
        val jar = new File(settings.downloadDir, project.name)

        if (!jar.exists()) {
          import sys.process._
          (project.url #> jar).!!
        }

        Await.result(engine.indexEntities(extractor(jar)), 1.hour)
      }
    }
  }
}

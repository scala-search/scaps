package scala.tools.apiSearch.benchmark

import java.io.File
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Calendar
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.sys.process.urlToProcess
import scala.tools.apiSearch.evaluation.stats.QueryStats
import scala.tools.apiSearch.featureExtraction.CompilerUtils
import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.tools.apiSearch.searchEngine.SearchEngine
import scala.tools.apiSearch.settings.Settings
import scala.tools.apiSearch.utils.using
import scalaz._
import scalaz.Validation
import scalaz.std.list._
import scalaz.syntax.validation._
import scalaz.syntax.traverse._
import scala.tools.apiSearch.evaluation.stats.Stats

object Benchmark extends App {
  val settings = Settings.fromApplicationConf
  val validationSettings = ValidationSettings.fromApplicationConf

  val engine = SearchEngine(settings).get

  initEnvironment(validationSettings, engine)

  val stats = validationSettings.queries.map {
    case (query, relevant) =>
      engine.search(query).get.map(
        results => QueryStats(query, results.map(_.withoutComment.toString()), relevant))
  }.sequenceU.map(Stats(_))

  stats.fold(
    errors => println(errors),
    stats => {
      stats.queryStats.foreach { qs => println(qs.query); println(qs); println() }
      println(stats)
    })

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

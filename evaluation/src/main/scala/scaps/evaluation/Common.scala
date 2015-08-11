package scaps.evaluation

import java.io.File
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.sys.process.urlToProcess
import scalaz.{ \/ => \/ }
import scalaz.std.list.listInstance
import scalaz.std.stream.streamInstance
import scalaz.syntax.traverse.ToTraverseOps
import scaps.evaluation.stats.QueryStats
import scaps.evaluation.stats.Stats
import scaps.featureExtraction.CompilerUtils
import scaps.featureExtraction.ExtractionError
import scaps.featureExtraction.JarExtractor
import scaps.searchEngine.QueryError
import scaps.searchEngine.SearchEngine
import scaps.settings.Settings
import scaps.utils.Logging
import scaps.webapi.Module
import scaps.utils.timers

object Common extends Logging {
  def runQueries(engine: SearchEngine, queriesWithRelevantDocs: List[(String, Set[String])]): QueryError \/ Stats = {
    queriesWithRelevantDocs.map {
      case (query, relevantResults) =>
        val (res, dur) = timers.withTime(engine.search(query).get)
        res.map(results => QueryStats(query, results.map(_.signature), relevantResults, dur))
    }.sequenceU.map(Stats(_))
  }

  def initSearchEngine(settings: Settings, evaluationSettings: EvaluationSettings) = {
    val engine = SearchEngine(settings).get
    if (evaluationSettings.rebuildIndex) {
      evaluationSettings.downloadDir.mkdirs()

      val classPaths = for {
        project <- evaluationSettings.projects
        dependency <- project.dependencies
      } yield {
        val file = new File(evaluationSettings.downloadDir, dependency.name)

        if (!file.exists()) {
          import sys.process._
          (dependency.url #> file).!!
        }

        file.getAbsolutePath()
      }

      CompilerUtils.withCompiler(classPaths) { compiler =>
        val extractor = new JarExtractor(compiler)

        engine.resetIndexes().get

        val modules = evaluationSettings.projects.map { project =>
          val jar = new File(evaluationSettings.downloadDir, project.name)

          if (!jar.exists()) {
            import sys.process._
            (project.url #> jar).!!
          }

          (Module("", project.name, ""), () => ExtractionError.logErrors(extractor(jar), logger.info(_)))
        }

        engine.indexEntities(modules).get
      }
    }
    engine
  }

  def updateSearchEngine(engine: SearchEngine, newSettings: Settings) = {
    if (engine.settings.index != newSettings.index) {
      println("Index time settings have changed!")

      val entities = engine.termsIndex.allEntities().get ++ engine.classesIndex.allEntities().get
      val newEngine = {
        val e = SearchEngine(newSettings).get
        e.resetIndexes().get
        e
      }
      newEngine.indexEntities(Module.Unknown, entities.toStream).get

      newEngine
    } else {
      SearchEngine(newSettings).get
    }
  }
}

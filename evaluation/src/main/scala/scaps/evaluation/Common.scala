package scaps.evaluation

import java.io.File

import scala.sys.process.urlToProcess

import scalaz.{ \/ => \/ }
import scalaz.std.list.listInstance
import scalaz.std.stream.streamInstance
import scalaz.syntax.traverse.ToTraverseOps
import scaps.api.Module
import scaps.evaluation.stats.QueryStats
import scaps.evaluation.stats.Stats
import scaps.scala.featureExtraction.CompilerUtils
import scaps.scala.featureExtraction.ExtractionError
import scaps.scala.featureExtraction.JarExtractor
import scaps.searchEngine.QueryError
import scaps.searchEngine.SearchEngine
import scaps.settings.Settings
import scaps.utils.Logging
import scaps.utils.timers

object Common extends Logging {
  def runQueries(engine: SearchEngine, settings: Settings, queriesWithRelevantDocs: List[(String, Set[String])]): QueryError \/ Stats = {
    queriesWithRelevantDocs.zipWithIndex.map {
      case ((query, relevantResults), idx) =>
        val (res, dur) = timers.withTime(engine.search(query).get)
        res.map { results =>
          QueryStats(idx, query, results.map(_.entity.signature), relevantResults, dur)
        }
    }.sequenceU.map(Stats(_, settings))
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

      val compiler = CompilerUtils.createCompiler(classPaths)
      val extractor = new JarExtractor(compiler)

      engine.resetIndexes().get

      evaluationSettings.projects.foreach { project =>
        val jar = new File(evaluationSettings.downloadDir, project.name)

        if (!jar.exists()) {
          import sys.process._
          (project.url #> jar).!!
        }

        val defs = ExtractionError.logErrors(extractor(jar), logger.info(_))

        engine.index(defs)
      }
    }
    engine.finalizeIndex().get
    engine
  }

  def updateSearchEngine(engine: SearchEngine, newSettings: Settings) = {
    if (engine.settings.index != newSettings.index) {
      println("Index time settings have changed!")

      val entities =
        engine.valueIndex.allEntities().get ++
          engine.typeIndex.allEntities().get ++
          engine.viewIndex.allEntities().get
      val newEngine = {
        val e = SearchEngine(newSettings).get
        e.resetIndexes().get
        e
      }
      newEngine.index(entities).get
      newEngine.finalizeIndex().get

      newEngine
    } else {
      SearchEngine(newSettings).get
    }
  }
}

package scala.tools.apiSearch.evaluation

import java.io.File

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.sys.process.urlToProcess
import scala.tools.apiSearch.evaluation.stats.QueryStats
import scala.tools.apiSearch.evaluation.stats.Stats
import scala.tools.apiSearch.featureExtraction.CompilerUtils
import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.tools.apiSearch.searchEngine.QueryError
import scala.tools.apiSearch.searchEngine.SearchEngine
import scala.tools.apiSearch.settings.Settings

import scalaz._
import scalaz.ValidationNel
import scalaz.std.list.listInstance
import scalaz.syntax.traverse.ToTraverseOps

object Common {
  def runQueries(engine: SearchEngine, queriesWithRelevantDocs: List[(String, Set[String])]): ValidationNel[QueryError, Stats] = {
    queriesWithRelevantDocs.map {
      case (query, relevantResults) =>
        engine.search(query).get.map(
          results => QueryStats(query, results.map(_.withoutComment.toString()), relevantResults))
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

      val compiler = CompilerUtils.initCompiler(classPaths)
      val extractor = new JarExtractor(compiler)

      engine.reset().get

      evaluationSettings.projects.foreach { project =>
        val jar = new File(evaluationSettings.downloadDir, project.name)

        if (!jar.exists()) {
          import sys.process._
          (project.url #> jar).!!
        }

        Await.result(engine.indexEntities(extractor(jar)), 1.hour)
      }
    }
    engine
  }
}

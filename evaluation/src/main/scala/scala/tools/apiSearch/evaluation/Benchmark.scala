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
import scala.tools.apiSearch.searchEngine.SearchEngine
import scala.tools.apiSearch.settings.Settings

import scalaz.std.list.listInstance
import scalaz.syntax.traverse.ToTraverseOps

object Benchmark extends App {
  val settings = Settings.fromApplicationConf
  val evaluationSettings = EvaluationSettings.fromApplicationConf

  val engine = Common.initSearchEngine(settings, evaluationSettings)

  Common.runQueries(engine, evaluationSettings.queries).fold(
    errors => println(errors),
    stats => {
      stats.queryStats.foreach { qs => println(qs.query); println(qs); println() }
      println(stats)
    })
}

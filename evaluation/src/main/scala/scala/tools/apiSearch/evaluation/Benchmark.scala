package scala.tools.apiSearch.evaluation

import java.io.File
import scala.collection.JavaConverters
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
import java.util.Calendar
import java.text.SimpleDateFormat
import java.io.Writer
import java.io.PrintWriter
import java.io.FileWriter
import scala.tools.apiSearch.utils.using

object Benchmark extends App {
  val outPath = args.lift(0)
  val runName = args.lift(1).getOrElse("")

  val settings = Settings.fromApplicationConf
  val evaluationSettings = EvaluationSettings.fromApplicationConf.copy(rebuildIndex = true)

  val engine = Common.initSearchEngine(settings, evaluationSettings)

  val now = (new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")).format(Calendar.getInstance.getTime())

  val cells = now :: runName :: Common.runQueries(engine, evaluationSettings.queries).fold(
    error => {
      println(error)
      "---" :: "---" :: Nil
    },
    stats => {
      stats.noQueries :: stats.meanAveragePrecision :: Nil
    })

  val csvRow = cells.mkString("", "; ", ";\n")

  println(csvRow)
  withWriter { writer => writer.write(csvRow) }

  def withWriter(f: Writer => Unit) = {
    outPath.fold {
      f(new PrintWriter(System.out))
    } { p =>
      val name = System.getProperty("user.home") + "/" + p
      val file = new File(name)
      file.createNewFile()
      using(new FileWriter(file, true))(f).get
    }
  }

}

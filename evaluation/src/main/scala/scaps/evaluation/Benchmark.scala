package scaps.evaluation

import java.io.File
import scala.collection.JavaConverters
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.sys.process.urlToProcess
import scaps.featureExtraction.CompilerUtils
import scaps.featureExtraction.JarExtractor
import scaps.searchEngine.SearchEngine
import scaps.settings.Settings
import scalaz.std.list.listInstance
import scalaz.syntax.traverse.ToTraverseOps
import java.util.Calendar
import java.text.SimpleDateFormat
import java.io.Writer
import java.io.PrintWriter
import java.io.FileWriter
import scaps.utils.using

object Benchmark extends App {
  val outPath = args.lift(0)
  val runName = args.lift(1).getOrElse("")

  val settings = Settings.fromApplicationConf
  val evaluationSettings = EvaluationSettings.fromApplicationConf

  val engine = Common.initSearchEngine(settings, evaluationSettings)

  val now = (new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")).format(Calendar.getInstance.getTime())
  val runInfo = now :: runName :: Nil

  val entries = Common.runQueries(engine, evaluationSettings.queries).fold(
    error => {
      println(error)
      List(runInfo ::: "<MAP>" :: "---" :: Nil)
    },
    stats => {
      val queryData = stats.queryStats.map { qs =>
        runInfo ::: qs.query :: qs.averagePrecision.toString :: Nil
      }
      queryData ::: List(runInfo ::: "<MAP>" :: stats.meanAveragePrecision.toString :: Nil)
    })

  val csvRows = entries.map(_.mkString("", "; ", ";"))
  val csv = csvRows.mkString("", "\n", "\n")

  println(csv)
  withWriter { writer => writer.write(csv) }

  def withWriter(f: Writer => Unit) = {
    outPath.fold {
      ()
    } { p =>
      val name = System.getProperty("user.home") + "/" + p
      val file = new File(name)
      file.createNewFile()
      using(new FileWriter(file, true))(f).get
    }
  }
}

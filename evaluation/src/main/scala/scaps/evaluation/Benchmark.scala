package scaps.evaluation

import java.io.File
import java.io.FileWriter
import java.io.Writer
import java.text.SimpleDateFormat
import java.util.Calendar

import scaps.settings.Settings
import scaps.utils.using

object Benchmark extends App {
  val outPath = args.lift(0)
  val runName = args.lift(1).getOrElse("")

  val settings = Settings.fromApplicationConf.modQuery(_.copy(explainScores = false))
  val evaluationSettings = EvaluationSettings.fromApplicationConf

  val engine = Common.initSearchEngine(settings, evaluationSettings)

  val now = (new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")).format(Calendar.getInstance.getTime())
  val runInfo = now :: runName :: Nil

  val entries = Common.runQueries(engine, evaluationSettings.queries).fold(
    error => {
      println(error)
      List(runInfo ::: "<MAP>" :: "---" :: "---" :: "---" :: Nil)
    },
    stats => {
      val queryData = stats.queryStats.map { qs =>
        runInfo ::: qs.query :: qs.averagePrecision :: qs.recallAt10 :: qs.duration.toMillis :: Nil
      }
      queryData ::: List(runInfo ::: "<MAP>" :: stats.meanAveragePrecision :: stats.meanRecallAt10 :: stats.duration.toMillis :: Nil)
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

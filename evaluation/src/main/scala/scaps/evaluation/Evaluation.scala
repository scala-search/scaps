/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.evaluation

import com.nicta.rng.Rng
import scaps.settings.Settings
import scaps.settings.QuerySettings
import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import java.io.FileWriter
import scaps.utils.using
import scaps.evaluation.stats.Stats
import scala.util.Random

object Evaluation extends App {
  import QuerySettings._
  import RngExtensions._

  val seed = Math.pow(42, 42).toLong

  val outputDir = "evaluation/target/results"

  val evaluationSettings = EvaluationSettings.fromApplicationConf

  val baseSettings = Settings.fromApplicationConf
    .modQuery(_.copy(
      maxResults = 100,
      views = false,
      fingerprintFrequencyCutoff = 0.8))

  val baseRngs = Map[String, Rng[Double]]().withDefaultValue(Rng.oneof(0d))

  // (instance name, number of configurations tested, configuration generator)
  val runs: List[(String, Int, Rng[Settings])] = List(
    ("I0: Baseline", 20, randomize(
      baseSettings
        .modIndex(_.copy(
          polarizedTypes = false))
        .modQuery(_.copy(
          views = false,
          fractions = false)),
      baseRngs ++ Map(
        penaltyWeight -> Rng.oneof(0d),
        docBoost -> Rng.choosedouble(0, 0.5),
        typeFrequencyWeight -> Rng.oneof(0d),
        distanceWeight -> Rng.oneof(0d)))),
    ("I1: Baseline + Polarized", 20, randomize(
      baseSettings
        .modIndex(_.copy(
          polarizedTypes = true))
        .modQuery(_.copy(
          views = false,
          fractions = false)),
      baseRngs ++ Map(
        penaltyWeight -> Rng.oneof(0d),
        docBoost -> Rng.choosedouble(0, 0.5),
        typeFrequencyWeight -> Rng.oneof(0d),
        distanceWeight -> Rng.oneof(0d)))),
    ("I2: Weighted", 100, randomize(
      baseSettings
        .modIndex(_.copy(
          polarizedTypes = false))
        .modQuery(_.copy(
          views = false,
          fractions = true)),
      baseRngs ++ Map(
        penaltyWeight -> Rng.choosedouble(0, 0.3),
        docBoost -> Rng.choosedouble(0, 0.5),
        typeFrequencyWeight -> Rng.oneof(math.E),
        distanceWeight -> Rng.oneof(0d)))),
    ("I3: Weighted + Polarized", 100, randomize(
      baseSettings
        .modIndex(_.copy(
          polarizedTypes = true))
        .modQuery(_.copy(
          views = false,
          fractions = true)),
      baseRngs ++ Map(
        penaltyWeight -> Rng.choosedouble(0, 0.3),
        docBoost -> Rng.choosedouble(0, 0.5),
        typeFrequencyWeight -> Rng.oneof(math.E),
        distanceWeight -> Rng.oneof(0d)))),
    ("I4: FEM", 200, randomize(
      baseSettings
        .modIndex(_.copy(
          polarizedTypes = true))
        .modQuery(_.copy(
          views = true,
          fractions = true)),
      baseRngs ++ Map(
        penaltyWeight -> Rng.choosedouble(0, 0.3),
        docBoost -> Rng.choosedouble(0, 0.5),
        typeFrequencyWeight -> Rng.oneof(Math.E),
        distanceWeight -> Rng.choosedouble(0, 0.3)))))

  val (trainingQueries, testQueries) =
    new Random(seed).shuffle(evaluationSettings.queries).splitAt((evaluationSettings.queries.length * 0.5).toInt)

  var engine = Common.initSearchEngine(baseSettings, evaluationSettings)

  val headers = List(
    "rid",
    "run",
    "polarized-types",
    QuerySettings.views,
    QuerySettings.fractions,
    QuerySettings.penaltyWeight,
    QuerySettings.typeFrequencyWeight,
    QuerySettings.distanceWeight,
    QuerySettings.docBoost,
    QuerySettings.fingerprintFrequencyCutoff,
    "MAP",
    "R@5",
    "R@10",
    "t")

  println(headers.mkString("; "))
  appendLinesTo(outputFile) {
    List(headers.mkString("; "))
  }

  val queryHeaders = List("rid", "run", "qid", "query", "ranks", "AP", "R@5", "R@10")
  appendLinesTo(queryDetailsOutputFile) {
    List(queryHeaders.mkString("; "))
  }

  runs.zipWithIndex.foreach {
    case ((runName, noConfigurations, settingsGenerator), ridx) =>
      println(s"start '$runName'")
      val allStats = settingsGenerator.fill(noConfigurations).runUnsafe(seed).map { settings =>
        engine = Common.updateSearchEngine(engine, settings)
        Common.runQueries(engine, settings, trainingQueries).fold(
          errors => {
            println(errors)
            ???
          },
          stats => {
            val cells = List[Any](
              ridx,
              runName,
              settings.index.polarizedTypes,
              settings.query.views,
              settings.query.fractions,
              settings.query.penaltyWeight,
              settings.query.typeFrequencyWeight,
              settings.query.distanceWeight,
              settings.query.docBoost,
              settings.query.fingerprintFrequencyCutoff,
              stats.meanAveragePrecision,
              stats.meanRecallAt(5),
              stats.meanRecallAt(10),
              stats.meanDuration.toMillis)

            println(cells.mkString("; "))
            appendLinesTo(outputFile) {
              List(cells.mkString("; "))
            }

            (stats, cells)
          })
      }

      val run = RunStats(runName, allStats)
      val bestByMap = run.topByMAP._1

      appendLinesTo(statsOutputFile) {
        List(run.toString)
      }

      appendLinesTo(queryDetailsOutputFile) {
        bestByMap.queryStats.map { qs =>
          val cells = List[Any](
            ridx,
            runName,
            qs.id,
            qs.query,
            qs.relRanks.map(_._2.map(_.toString).getOrElse("-")).mkString(", "),
            qs.averagePrecision,
            qs.recallAt(5),
            qs.recallAt(10))

          cells.mkString("; ")
        }
      }

      val validationStats = {
        val validationSettings = bestByMap.settings
        engine = Common.updateSearchEngine(engine, validationSettings)
        Common.runQueries(engine, validationSettings, testQueries)
      }.getOrElse(???)

      appendLinesTo(statsOutputFile) {
        List(validationStats.toString)
      }

      appendLinesTo(queryDetailsOutputFile) {
        validationStats.queryStats.map { qs =>
          val cells = List[Any](
            ridx,
            "Validation " + runName,
            qs.id,
            qs.query,
            qs.relRanks.map(_._2.map(_.toString).getOrElse("-")).mkString(", "),
            qs.averagePrecision,
            qs.recallAt(5),
            qs.recallAt(10))

          cells.mkString("; ")
        }
      }
  }

  def randomize(settings: Settings, rngs: Map[String, Rng[Double]]): Rng[Settings] =
    for {
      query <- randomize(settings.query, rngs)
    } yield settings.copy(query = query)

  def randomize(settings: QuerySettings, rngs: Map[String, Rng[Double]]): Rng[QuerySettings] =
    for {
      pw <- rngs(penaltyWeight)
      tf <- rngs(typeFrequencyWeight)
      dw <- rngs(distanceWeight)
      db <- rngs(docBoost)
    } yield settings.copy(
      penaltyWeight = pw,
      typeFrequencyWeight = tf,
      distanceWeight = dw,
      docBoost = db)

  lazy val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
  lazy val now = Calendar.getInstance.getTime

  def outputFile() = {
    new File(outputDir).mkdirs()
    new File(s"$outputDir/evaluation-${format.format(now)}.csv")
  }

  def statsOutputFile() = {
    new File(outputDir).mkdirs()
    new File(s"$outputDir/evaluation-stats-${format.format(now)}.txt")
  }

  def queryDetailsOutputFile() = {
    new File(outputDir).mkdirs()
    new File(s"$outputDir/evaluation-queries-${format.format(now)}.csv")
  }

  def appendLinesTo(f: File)(ls: List[String]) =
    using(new FileWriter(f, true)) { w => ls.foreach { l => w.write(l + "\n") } }.get
}

case class RunStats(name: String, stats: List[(Stats, List[Any])]) {
  def topByMAP = stats.maxBy(_._1.meanAveragePrecision)
  def topByR10 = stats.maxBy(_._1.meanRecallAt(10))
  def avgMAP = stats.map(_._1.meanAveragePrecision).sum / stats.length
  def avgR10 = stats.map(_._1.meanRecallAt(10)).sum / stats.length
  def avgRuntime = stats.map(_._1.meanDuration).reduce(_ + _) / stats.length

  override def toString =
    s"""
      |${name}
      |  top by MAP: ${topByMAP._2}
      |  top by R@10: ${topByR10._2}
      |  avg. MAP: ${avgMAP}
      |  avg. R@10: ${avgR10}
      |  avg. runtime: ${avgRuntime.toMillis}
      |""".stripMargin
}

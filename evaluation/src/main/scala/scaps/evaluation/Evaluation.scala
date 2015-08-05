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

object Evaluation extends App {
  import QuerySettings._
  import RngExtensions._

  val outputDir = "evaluation/target/results"

  val evaluationSettings = EvaluationSettings.fromApplicationConf

  val baseSettings = Settings.fromApplicationConf.modQuery(_.copy(
    maxResults = 100,
    views = false,
    fractions = false,
    fingerprintFrequencyCutoff = 0.8))

  val baseRngs = Map[String, Rng[Double]](
    lengthNormWeight -> Rng.choosedouble(0, 0.5),
    nameBoost -> Rng.oneof(0.1),
    docBoost -> Rng.oneof(0.05))
    .withDefaultValue(Rng.oneof(0d))

  // (name, number of configurations tested, configuration generator)
  val runs: List[(String, Int, Rng[Settings])] = List(
    ("Baseline", 50, randomize(
      baseSettings,
      baseRngs ++ Map(
        nameBoost -> Rng.oneof(0.1),
        docBoost -> Rng.choosedouble(0, 0.5)))),
    ("Baseline+All", 500, randomize(
      baseSettings.modQuery(_.copy(
        fractions = true)),
      baseRngs ++ Map(
        depthBoostWeight -> Rng.choosedouble(0, 2),
        typeFrequencyWeight -> Rng.choosedouble(0, 2)))),
    ("FEM", 50, randomize(
      baseSettings.modQuery(_.copy(
        views = true)),
      baseRngs)),
    ("FEM+Di", 50, randomize(
      baseSettings.modQuery(_.copy(
        views = true)),
      baseRngs ++ Map(
        distanceBoostWeight -> Rng.choosedouble(0, 2)))),
    ("FEM+De", 50, randomize(
      baseSettings.modQuery(_.copy(
        views = true)),
      baseRngs ++ Map(
        depthBoostWeight -> Rng.choosedouble(0, 2)))),
    ("FEM+TF", 50, randomize(
      baseSettings.modQuery(_.copy(
        views = true)),
      baseRngs ++ Map(
        typeFrequencyWeight -> Rng.choosedouble(0, 2)))),
    ("FEM+Fr", 50, randomize(
      baseSettings.modQuery(_.copy(
        views = true,
        fractions = true)),
      baseRngs)),
    ("FEM-Di", 500, randomize(
      baseSettings.modQuery(_.copy(
        views = true,
        fractions = true)),
      baseRngs ++ Map(
        depthBoostWeight -> Rng.choosedouble(0, 2),
        typeFrequencyWeight -> Rng.choosedouble(0, 2)))),
    ("FEM-De", 500, randomize(
      baseSettings.modQuery(_.copy(
        views = true,
        fractions = true)),
      baseRngs ++ Map(
        distanceBoostWeight -> Rng.choosedouble(0, 2),
        typeFrequencyWeight -> Rng.choosedouble(0, 2)))),
    ("FEM-TF", 500, randomize(
      baseSettings.modQuery(_.copy(
        views = true,
        fractions = true)),
      baseRngs ++ Map(
        distanceBoostWeight -> Rng.choosedouble(0, 2),
        depthBoostWeight -> Rng.choosedouble(0, 2)))),
    ("FEM-Fr", 500, randomize(
      baseSettings.modQuery(_.copy(
        views = true,
        fractions = false)),
      baseRngs ++ Map(
        distanceBoostWeight -> Rng.choosedouble(0, 2),
        depthBoostWeight -> Rng.choosedouble(0, 2),
        typeFrequencyWeight -> Rng.choosedouble(0, 2)))),
    ("FEM+All", 500, randomize(
      baseSettings.modQuery(_.copy(
        views = true,
        fractions = true)),
      baseRngs ++ Map(
        distanceBoostWeight -> Rng.choosedouble(0, 2),
        depthBoostWeight -> Rng.choosedouble(0, 2),
        typeFrequencyWeight -> Rng.choosedouble(0, 2)))))

  var engine = Common.initSearchEngine(baseSettings, evaluationSettings)

  using(new FileWriter(outputFile)) { writer =>
    val headers = List(
      "run",
      QuerySettings.views,
      QuerySettings.fractions,
      QuerySettings.lengthNormWeight,
      QuerySettings.distanceBoostWeight,
      QuerySettings.depthBoostWeight,
      QuerySettings.typeFrequencyWeight,
      QuerySettings.nameBoost,
      QuerySettings.docBoost,
      QuerySettings.fingerprintFrequencyCutoff,
      "MAP",
      "R@10",
      "t")

    println(headers.mkString("", "; ", ";\n"))
    writer.write(headers.mkString("", "; ", ";\n"))

    runs.foreach {
      case (runName, noConfigurations, settingsGenerator) =>
        println(s"start '$runName'")
        val allStats = settingsGenerator.fill(noConfigurations).runUnsafe(Math.pow(42, 42).toLong).map { settings =>
          engine = Common.updateSearchEngine(engine, settings)
          Common.runQueries(engine, evaluationSettings.queries).fold(
            errors => {
              println(errors)
              ???
            },
            stats => {
              val cells = List[Any](
                runName,
                settings.query.views,
                settings.query.fractions,
                settings.query.lengthNormWeight,
                settings.query.distanceBoostWeight,
                settings.query.depthBoostWeight,
                settings.query.typeFrequencyWeight,
                settings.query.nameBoost,
                settings.query.docBoost,
                settings.query.fingerprintFrequencyCutoff,
                stats.meanAveragePrecision,
                stats.meanRecallAt10,
                stats.duration.toMillis + " ms")

              println(cells.mkString("", "; ", ";\n"))
              writer.write(cells.mkString("", "; ", ";\n"))

              (stats, cells)
            })
        }

        using(new FileWriter(statsOutputFile, true)) { statsWriter =>
          val run = RunStats(runName, allStats)
          println(run)
          statsWriter.write(run.toString)
        }
    }
  }.get

  def randomize(settings: Settings, rngs: Map[String, Rng[Double]]): Rng[Settings] =
    for {
      query <- randomize(settings.query, rngs)
    } yield settings.copy(query = query)

  def randomize(settings: QuerySettings, rngs: Map[String, Rng[Double]]): Rng[QuerySettings] =
    for {
      lnw <- rngs(lengthNormWeight)
      dist <- rngs(distanceBoostWeight)
      depth <- rngs(depthBoostWeight)
      tf <- rngs(typeFrequencyWeight)
      nb <- rngs(nameBoost)
      db <- rngs(docBoost)
    } yield settings.copy(
      lengthNormWeight = lnw,
      distanceBoostWeight = dist,
      depthBoostWeight = depth,
      typeFrequencyWeight = tf,
      nameBoost = nb,
      docBoost = db)

  lazy val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
  lazy val now = Calendar.getInstance.getTime

  def outputFile() = {
    new File(outputDir).mkdirs()
    new File(s"$outputDir/evaluation-${format.format(now)}.csv")
  }

  def statsOutputFile() = {
    new File(outputDir).mkdirs()
    new File(s"$outputDir/evaluation-stats-${format.format(now)}.csv")
  }
}

case class RunStats(name: String, stats: List[(Stats, List[Any])]) {
  def topByMAP = stats.maxBy(_._1.meanAveragePrecision)
  def topByR10 = stats.maxBy(_._1.meanRecallAt10)
  def avgMAP = stats.map(_._1.meanAveragePrecision).sum / stats.length
  def avgR10 = stats.map(_._1.meanRecallAt10).sum / stats.length
  def avgRuntime = stats.map(_._1.queryStats.map(_.duration).reduce(_ + _)).reduce(_ + _) / stats.length

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

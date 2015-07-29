package scaps.evaluation

import com.nicta.rng.Rng
import scaps.settings.Settings
import scaps.settings.QuerySettings
import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import java.io.FileWriter
import scaps.utils.using

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
    ("baseline", 100, randomize(
      baseSettings,
      baseRngs ++ Map(
        nameBoost -> Rng.oneof(0.1),
        docBoost -> Rng.choosedouble(0, 0.5)))),
    ("pure FEM", 100, randomize(
      baseSettings.modQuery(_.copy(
        views = true)),
      baseRngs)),
    ("FEM + Distance", 100, randomize(
      baseSettings.modQuery(_.copy(
        views = true)),
      baseRngs ++ Map(
        distanceBoostWeight -> Rng.choosedouble(0, 2)))),
    ("FEM + Depth", 100, randomize(
      baseSettings.modQuery(_.copy(
        views = true)),
      baseRngs ++ Map(
        depthBoostWeight -> Rng.choosedouble(0, 2)))),
    ("FEM + Type Frequency", 100, randomize(
      baseSettings.modQuery(_.copy(
        views = true)),
      baseRngs ++ Map(
        typeFrequencyWeight -> Rng.choosedouble(0, 2)))),
    ("FEM + Fractions", 100, randomize(
      baseSettings.modQuery(_.copy(
        views = true,
        fractions = true)),
      baseRngs)),
    ("All Extensions", 1000, randomize(
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
      QuerySettings.depthBoostWeight,
      QuerySettings.distanceBoostWeight,
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
        settingsGenerator.fill(noConfigurations).runUnsafe(Math.pow(42, 42).toLong).foreach { settings =>
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
                settings.query.depthBoostWeight,
                settings.query.distanceBoostWeight,
                settings.query.typeFrequencyWeight,
                settings.query.nameBoost,
                settings.query.docBoost,
                settings.query.fingerprintFrequencyCutoff,
                stats.meanAveragePrecision,
                stats.meanRecallAt10,
                stats.duration.toMillis + " ms")

              println(cells.mkString("", "; ", ";\n"))
              writer.write(cells.mkString("", "; ", ";\n"))
            })
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
      depth <- rngs(depthBoostWeight)
      dist <- rngs(distanceBoostWeight)
      tf <- rngs(typeFrequencyWeight)
      nb <- rngs(nameBoost)
      db <- rngs(docBoost)
    } yield settings.copy(
      lengthNormWeight = lnw,
      depthBoostWeight = depth,
      distanceBoostWeight = dist,
      typeFrequencyWeight = tf,
      nameBoost = nb,
      docBoost = db)

  def outputFile() = {
    val output = new File(outputDir)
    output.mkdirs()
    val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val now = Calendar.getInstance.getTime
    new File(s"$outputDir/evaluation-${format.format(now)}.csv")
  }
}

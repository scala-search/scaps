package scaps.evaluation

import java.io.File
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Calendar

import scaps.settings.IndexSettings
import scaps.settings.QuerySettings
import scaps.settings.Settings
import scaps.utils.using

import com.nicta.rng.Rng
import com.nicta.rng.Size.IntSize

import RngExtensions.RichRng

object FindParameters extends App {
  val outputDir = "evaluation/target/results"

  val lengthNormWeights = Rng.choosedouble(0, 1)
  val parameterCountWeight = Rng.choosedouble(0, 1)
  val depthBoostWeight = Rng.choosedouble(0, 2)
  val distanceBoostWeight = Rng.choosedouble(0, 3)
  val typeFrequencyWeight = Rng.choosedouble(0, 3)
  val nameBoosts = Rng.oneof(0.02)
  val docBoosts = Rng.oneof(0.01)
  val fingerprintFrequencyCutoff = Rng.oneof(0.5)

  val noConfigurations = 5000

  val settings = Settings.fromApplicationConf
  val evaluationSettings = EvaluationSettings.fromApplicationConf.copy(rebuildIndex = true)

  var engine = Common.initSearchEngine(settings, evaluationSettings)

  using(new FileWriter(outputFile)) { writer =>
    val headers = List(
      QuerySettings.lengthNormWeight,
      QuerySettings.parameterCountWeight,
      QuerySettings.depthBoostWeight,
      QuerySettings.distanceBoostWeight,
      QuerySettings.typeFrequencyWeight,
      QuerySettings.nameBoost,
      QuerySettings.docBoost,
      QuerySettings.fingerprintFrequencyCutoff,
      "MAP",
      "R@10",
      "t")

    writer.write(headers.mkString("", "; ", ";\n"))

    generateConfs(noConfigurations, Math.pow(42d, 42d).toLong, settings).zipWithIndex.foreach {
      case (settings, idx) =>
        println(s"test configuration ${idx} of $noConfigurations")

        engine = Common.updateSearchEngine(engine, settings)
        Common.runQueries(engine, evaluationSettings.queries).fold(
          errors => {
            println(errors)
            ???
          },
          stats => {
            val cells = List[Any](
              settings.query.lengthNormWeight,
              settings.query.parameterCountWeight,
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
  }.get

  def generateConfs(size: Int, seed: Long, settings: Settings): Seq[Settings] =
    randomize(settings)
      .fill(size)
      .runUnsafe(seed)

  def randomize(settings: Settings): Rng[Settings] =
    for {
      query <- randomize(settings.query)
    } yield settings.copy(query = query)

  def randomize(settings: QuerySettings): Rng[QuerySettings] =
    for {
      lnw <- lengthNormWeights
      pcw <- parameterCountWeight
      depth <- depthBoostWeight
      dist <- distanceBoostWeight
      tf <- typeFrequencyWeight
      nb <- nameBoosts
      db <- docBoosts
      fpCutoff <- fingerprintFrequencyCutoff
    } yield settings.copy(
      lengthNormWeight = lnw,
      parameterCountWeight = pcw,
      depthBoostWeight = depth,
      distanceBoostWeight = dist,
      typeFrequencyWeight = tf,
      nameBoost = nb,
      docBoost = db,
      fingerprintFrequencyCutoff = fpCutoff)

  def outputFile() = {
    val output = new File(outputDir)
    output.mkdirs()
    val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val now = Calendar.getInstance.getTime
    new File(s"$outputDir/${format.format(now)}.csv")
  }
}

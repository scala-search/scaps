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

  val lengthNormWeights = Rng.oneof(0.7) //RngExtensions.normalSample(1d, 0.05d).map(d => (d * 10).round / 10d)

  val distanceBoostGradients = Rng.choosedouble(0, 2)
  val depthBoostGradients = Rng.choosedouble(0, 2)
  val idfWeights = Rng.oneof(0.35)
  val nameBoosts = Rng.oneof(0.2)
  val docBoosts = Rng.oneof(0.1)

  val noConfigurations = 200

  val settings = Settings.fromApplicationConf
  val evaluationSettings = EvaluationSettings.fromApplicationConf.copy(rebuildIndex = true)

  var engine = Common.initSearchEngine(settings, evaluationSettings)

  using(new FileWriter(outputFile)) { writer =>
    writer.write("lengthNormWeight; distanceBoostGradient; depthBoostGradient; idfWeight; nameBoost; docBoost; MAP;\n")

    generateConfs(noConfigurations, Math.pow(42d, 42d).toLong, settings).zipWithIndex.foreach {
      case (settings, idx) =>
        println(s"test configuration ${idx} of $noConfigurations")
        println(settings)
        engine = Common.updateSearchEngine(engine, settings)
        Common.runQueries(engine, evaluationSettings.queries).fold(
          errors => {
            println(errors)
            ???
          },
          stats => {
            println(stats.meanAveragePrecision)
            println()

            val cells = List(
              settings.index.lengthNormWeight,
              settings.query.distanceBoostGradient,
              settings.query.depthBoostGradient,
              settings.query.idfWeight,
              settings.query.nameBoost,
              settings.query.docBoost,
              stats.meanAveragePrecision)
            writer.write(cells.mkString("", "; ", ";\n"))
          })
    }
  }.get

  def generateConfs(size: Int, seed: Long, settings: Settings): Seq[Settings] =
    randomize(settings)
      .fill(size)
      .runUnsafe(seed)
      .sorted(Ordering[Double].on((s: Settings) => s.index.lengthNormWeight))

  def randomize(settings: Settings): Rng[Settings] =
    for {
      index <- randomize(settings.index)
      query <- randomize(settings.query)
    } yield settings.copy(index = index, query = query)

  def randomize(settings: QuerySettings): Rng[QuerySettings] =
    for {
      dist <- distanceBoostGradients
      depth <- depthBoostGradients
      idf <- idfWeights
      nb <- nameBoosts
      db <- docBoosts
    } yield settings.copy(distanceBoostGradient = dist, depthBoostGradient = depth, idfWeight = idf, nameBoost = nb, docBoost = db)

  def randomize(settings: IndexSettings): Rng[IndexSettings] =
    for {
      lnw <- lengthNormWeights
    } yield settings.copy(lengthNormWeight = lnw)

  def outputFile() = {
    val output = new File(outputDir)
    output.mkdirs()
    val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val now = Calendar.getInstance.getTime
    new File(s"$outputDir/${format.format(now)}.csv")
  }
}

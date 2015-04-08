package scala.tools.apiSearch.evaluation

import java.io.File
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.tools.apiSearch.settings.IndexSettings
import scala.tools.apiSearch.settings.QuerySettings
import scala.tools.apiSearch.settings.Settings
import scala.tools.apiSearch.settings.Instances._
import scala.tools.apiSearch.utils.using

import com.nicta.rng.Rng
import com.nicta.rng.Size.IntSize

import RngExtensions.RichRng

object FindParameters extends App {
  val outputDir = "evaluation/target/results"

  val lengthNormWeights = Rng.oneof(0.7) //RngExtensions.normalSample(1d, 0.05d).map(d => (d * 10).round / 10d)

  val distanceBoostGradients = Rng.oneof(1)
  val depthBoostGradients = Rng.oneof(0.2)
  val idfWeights = Rng.oneof(0.35)
  val nameBoosts = Rng.oneof(0.2)
  val docBoosts = Rng.oneof(0.1)

  val settings = Settings.fromApplicationConf
  val evaluationSettings = EvaluationSettings.fromApplicationConf.copy(rebuildIndex = false)

  var engine = Common.initSearchEngine(settings, evaluationSettings)

  using(new FileWriter(outputFile)) { writer =>
    writer.write("lengthNormWeight; distanceBoostGradient; depthBoostGradient; idfWeight; nameBoost; docBoost; MAP;\n")

    generateConfs(1, Math.pow(42d, 42d).toLong, settings).foreach {
      case settings =>
        println(settings)
        engine = Common.updateSearchEngine(engine, settings)
        val stats = Common.runQueries(engine, evaluationSettings.queries).getOrElse(???)
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
    }
  }

  def generateConfs(size: Int, seed: Long, settings: Settings): Seq[Settings] = {
    randomize(settings).fill(size).runUnsafe(seed).sorted(implicitly[Ordering[Settings]].reverse)
  }

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
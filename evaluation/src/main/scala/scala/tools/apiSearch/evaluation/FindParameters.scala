package scala.tools.apiSearch.evaluation

import scala.tools.apiSearch.settings._
import scala.util.Random
import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import scala.tools.apiSearch.utils.using
import java.io.FileWriter
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

object FindParameters extends App {
  val outputDir = "evaluation/target/results"

  val lengthNormWeights = List(0.5f, 0.8f, 1f, 1.2f, 1.5f)

  val distanceBoostWeights = List(0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1f)
  val depthBoostWeights = List(0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1f)
  val idfWeights = List(0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1f)
  val nameBoosts = List(0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1f)
  val docBoosts = List(0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1f)

  val settings = Settings.fromApplicationConf
  val evaluationSettings = EvaluationSettings.fromApplicationConf.copy(rebuildIndex = true)

  var engine = Common.initSearchEngine(settings, evaluationSettings)

  using(new FileWriter(outputFile)) { writer =>
    writer.write("lengthNormWeight; distanceBoostWeight; depthBoostWeight; idfWeight; nameBoost; docBoost; MAP;\n")

    generateConfs(3000, Math.pow(42, 42).toLong, settings)
      .foreach {
        case settings =>
          println(settings)
          engine = Common.updateSearchEngine(engine, settings)
          val stats = Common.runQueries(engine, evaluationSettings.queries).getOrElse(???)
          println(stats.meanAveragePrecision)
          println()

          val cells = List(
            settings.index.lengthNormWeight,
            settings.query.distanceBoostWeight,
            settings.query.depthBoostWeight,
            settings.query.idfWeight,
            settings.query.nameBoost,
            settings.query.docBoost,
            stats.meanAveragePrecision)
          writer.write(cells.mkString("", "; ", ";\n"))
      }
  }

  def generateConfs(n: Int, seed: Long, settings: Settings): List[Settings] = {
    val r = new Random(seed)

    val indexTimeSettings = indexTimeConfs(settings.index)

    for {
      index <- indexTimeSettings
      query <- r.shuffle(queryTimeConfs(settings.query)).take(n / indexTimeSettings.size)
    } yield (settings.copy(index = index, query = query))
  }

  def indexTimeConfs(settings: IndexSettings) =
    for {
      lnw <- lengthNormWeights
    } yield settings.copy(lengthNormWeight = lnw)

  def queryTimeConfs(settings: QuerySettings) =
    for {
      dist <- distanceBoostWeights
      depth <- depthBoostWeights
      idf <- idfWeights
      nb <- nameBoosts
      db <- docBoosts
    } yield settings.copy(distanceBoostWeight = dist, depthBoostWeight = depth, idfWeight = idf, nameBoost = nb, docBoost = db)

  def outputFile() = {
    val output = new File(outputDir)
    output.mkdirs()
    val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val now = Calendar.getInstance.getTime
    new File(s"$outputDir/${format.format(now)}.csv")
  }
}

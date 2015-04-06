package scala.tools.apiSearch.evaluation

import scala.tools.apiSearch.settings._

object FindParameters extends App {

  val lengthNormWeights = List(0.5f, 1f, 1.5f)

  val distanceBoostWeights = List(0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1f)
  val depthBoostWeights = List(0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1f)
  val idfWeights = List(0.01f, 0.02f, 0.05f, 0.1f)
  val nameBoosts = List(0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1f)
  val docBoosts = List(0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1f)

  val results = generateConfs(Settings.fromApplicationConf, EvaluationSettings.fromApplicationConf).map {
    case (settings, evaluationSettings) =>
      println(settings)
      println(evaluationSettings.rebuildIndex)
      val engine = Common.initSearchEngine(settings, evaluationSettings)
      val stats = Common.runQueries(engine, evaluationSettings.queries)
      println()

      (settings, stats)
  }

  println(results.sortBy(_._2.map(_.meanAveragePrecision).getOrElse(0d)).take(10))

  def generateConfs(settings: Settings, evaluationSettings: EvaluationSettings): List[(Settings, EvaluationSettings)] = {
    val indexTimeSettings = for {
      lnw <- lengthNormWeights
    } yield settings.index.copy(lengthNormWeight = lnw)

    val queryTimeSettings = for {
      dist <- distanceBoostWeights
      depth <- depthBoostWeights
      idf <- idfWeights
      nb <- nameBoosts
      db <- docBoosts
    } yield settings.query.copy(distanceBoostWeight = dist, depthBoostWeight = depth, idfWeight = idf, nameBoost = nb, docBoost = db)

    indexTimeSettings.flatMap { index =>
      var firstWithNewIndexSettings = true
      queryTimeSettings.map { query =>
        val es = evaluationSettings.copy(
          rebuildIndex = if (firstWithNewIndexSettings) true else false)

        firstWithNewIndexSettings = false
        (settings.copy(index = index, query = query), es)
      }
    }
  }
}

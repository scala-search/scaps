package scaps.evaluation.stats

case class Stats(queryStats: List[QueryStats]) {
  val noQueries = queryStats.size
  val meanAveragePrecision = queryStats.map(_.averagePrecision).sum / queryStats.size
  val geometricMeanAveragePrecision = Math.pow(queryStats.foldLeft(1d)(_ * _.averagePrecision), -queryStats.size)
  val meanRecallAt10 = queryStats.map(_.recallAt10).sum / queryStats.size
  val duration = queryStats.map(_.duration).reduce(_ + _)

  override def toString = s"no. queries: $noQueries, MAP: $meanAveragePrecision, gMAP: $geometricMeanAveragePrecision, P10: $meanRecallAt10"
}

object Stats {
  implicit val statsOrdering =
    Ordering[Double].on((s: Stats) => s.meanAveragePrecision)
}

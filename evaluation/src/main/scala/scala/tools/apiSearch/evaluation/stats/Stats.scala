package scala.tools.apiSearch.evaluation.stats

case class Stats(queryStats: List[QueryStats]) {
  val noQueries = queryStats.size
  val meanAveragePrecision = queryStats.foldLeft(0d)(_ + _.averagePrecision) / queryStats.size
  val geometricMeanAveragePrecision = Math.pow(queryStats.foldLeft(1d)(_ * _.averagePrecision), -queryStats.size)

  override def toString = s"no. queries: $noQueries, MAP: $meanAveragePrecision, gMAP: $geometricMeanAveragePrecision"
}

object Stats {
  implicit val statsOrdering =
    Ordering[Double].on((s: Stats) => s.meanAveragePrecision)
}

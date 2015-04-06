package scala.tools.apiSearch.evaluation.stats

case class Stats(queryStats: List[QueryStats]) {
  val noQueries = queryStats.size
  val meanAveragePrecision = queryStats.foldLeft(0d)(_ + _.averagePrecision) / queryStats.size

  override def toString = s"no. queries: $noQueries, map: $meanAveragePrecision"
}

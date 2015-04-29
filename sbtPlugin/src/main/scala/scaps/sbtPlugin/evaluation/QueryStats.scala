package scaps.sbtPlugin.evaluation

import scala.annotation.tailrec
import scala.Ordering

case class QueryStats(query: String, retrievedDocs: Int, relevantRetrievedDocs: Int, relevantDocs: Int, accumulatedPrecision: Double) {
  val recall = relevantRetrievedDocs.toDouble / relevantDocs
  val precision = QueryStats.precision(relevantRetrievedDocs, retrievedDocs)
  val averagePrecision = accumulatedPrecision / relevantDocs

  override def toString() =
    s"ret: $retrievedDocs, rel: $relevantDocs, relret: $relevantRetrievedDocs, recall: $recall, precision: $precision, accp: $accumulatedPrecision, avep: $averagePrecision"
}

object QueryStats {
  def precision(relret: Int, ret: Int): Double =
    if (ret == 0) 0d else (relret.toDouble / ret)

  def apply[A](query: String, results: Seq[A], relevant: Set[A]): QueryStats = {
    def next(prev: QueryStats, isRelevant: Boolean) = {
      val isRel = if (isRelevant) 1 else 0
      val ret = prev.retrievedDocs + 1
      val relret = prev.relevantRetrievedDocs + isRel
      prev.copy(
        retrievedDocs = ret,
        relevantRetrievedDocs = relret,
        accumulatedPrecision = prev.accumulatedPrecision + (QueryStats.precision(relret, ret) * isRel))
    }

    @tailrec
    def loop(results: Seq[A], prev: QueryStats): QueryStats = {
      if (results.isEmpty || prev.relevantRetrievedDocs >= prev.relevantDocs) prev
      else {
        loop(results.tail, next(prev, relevant(results.head)))
      }
    }

    loop(results, QueryStats(query, 0, 0, relevant.size, 0d))
  }

  implicit val queryStatsOrdering =
    Ordering[Double].on((qs: QueryStats) => qs.averagePrecision)
}

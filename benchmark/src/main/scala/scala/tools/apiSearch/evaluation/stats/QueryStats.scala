package scala.tools.apiSearch.evaluation.stats

import scala.annotation.tailrec

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

  def apply[A](query: String, results: Seq[A], relevant: Set[A]): QueryStats =
    apply(results, relevant, QueryStats(query, 0, 0, relevant.size, 0d))

  @tailrec
  private[stats] def apply[A](results: Seq[A], isRelevant: A => Boolean, prev: QueryStats): QueryStats = {
    if (results.isEmpty || prev.relevantRetrievedDocs >= prev.relevantDocs) prev
    else {
      apply(results.tail, isRelevant, next(prev, isRelevant(results.head)))
    }
  }

  private[stats] def next(prev: QueryStats, isRelevant: Boolean) = {
    val isRel = if (isRelevant) 1 else 0
    val ret = prev.retrievedDocs + 1
    val relret = prev.relevantRetrievedDocs + isRel
    prev.copy(
      retrievedDocs = ret,
      relevantRetrievedDocs = relret,
      accumulatedPrecision = prev.accumulatedPrecision + (QueryStats.precision(relret, ret) * isRel))
  }

  implicit object QueryStatsIsOrdering extends Ordering[QueryStats] {
    def compare(x: QueryStats, y: QueryStats): Int = {
      implicitly[Ordering[Double]].compare(x.averagePrecision, y.averagePrecision)
    }
  }
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.evaluation.stats

import scala.annotation.tailrec
import scala.Ordering
import scala.concurrent.duration.Duration
import scalaz.std.option._

case class QueryStats(
    id: Int,
    query: String,
    relRanks: Map[String, Option[Int]],
    retrievedDocs: Int,
    duration: Duration) {

  val relevantRetrievedDocs = relRanks.count(_._2.isDefined)
  val relevantDocs = relRanks.size

  val accumulatedPrecision = {
    val ranks = relRanks.flatMap(_._2).toSet

    (1 to retrievedDocs).map { k =>
      if (ranks(k))
        QueryStats.precision(ranks.count(_ <= k), k)
      else
        0d
    }.sum
  }

  val recall = relevantRetrievedDocs.toDouble / relevantDocs
  val precision = QueryStats.precision(relevantRetrievedDocs, retrievedDocs)
  val averagePrecision = accumulatedPrecision / relevantDocs

  def recallAt(n: Int) = relRanks.count(_._2.getOrElse(Int.MaxValue) <= n).toDouble / Math.min(relevantDocs, n)

  override def toString() =
    s"ret: $retrievedDocs, rel: $relevantDocs, relret: $relevantRetrievedDocs, recall: $recall, precision: $precision, accp: $accumulatedPrecision, avep: $averagePrecision"
}

object QueryStats {
  def precision(relret: Int, ret: Int): Double =
    if (ret == 0) 0d else (relret.toDouble / ret)

  def apply(id: Int, query: String, results: Seq[String], relevant: Set[String], duration: Duration): QueryStats = {
    val relRanks = relevant.map { res =>
      val idx = results.indexWhere { _ == res }
      res -> (if (idx == -1) None else Some(idx + 1))
    }.toMap
    QueryStats(id, query, relRanks, results.size, duration)
  }

  implicit val queryStatsOrdering =
    Ordering[Double].on((qs: QueryStats) => qs.averagePrecision)
}

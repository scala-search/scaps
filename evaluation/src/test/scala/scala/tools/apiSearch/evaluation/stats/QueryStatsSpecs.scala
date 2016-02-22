/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.evaluation.stats

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.path.FreeSpec
import scala.concurrent.duration.Duration

class QueryStatsSpecs extends FreeSpec with Matchers {
  "query stats" - {
    "should calculate average precisions" - {
      val results = 1 to 10

      "when all results are relevant" in {
        avep(results, results.toSet) should be(1d)
      }

      "when no result is relevant" in {
        avep(results, Set(100)) should be(0d)
      }

      "when first result is relevant" in {
        avep(results, Set(1)) should be(1d)
      }

      "when second result is relevant" in {
        avep(results, Set(2)) should be(0.5d)
      }

      "when first and one not-retrieved results are relevant" in {
        avep(results, Set(1, 100)) should be(0.5d)
      }

      "when last two results are relevant" in {
        avep(results, Set(9, 10)) should be((1d / 9d + 2d / 10d) / 2d)
      }
    }

    "should claculate recall in top 10" - {
      val results = 1 to 100

      "when all results are relevant" in {
        recall10(results, results.toSet) should be(1d)
      }

      "when no result is relevant" in {
        recall10(results, Set(1000)) should be(0d)
      }

      "when first result is relevant" in {
        recall10(results, Set(1)) should be(1d)
      }

      "when second result is relevant" in {
        recall10(results, Set(2)) should be(1d)
      }

      "when first and one not-retrieved results are relevant" in {
        recall10(results, Set(1, 1000)) should be(0.5d)
      }

      "when last two results in top 10 are relevant" in {
        recall10(results, Set(9, 10)) should be(1d)
      }
    }

    "should keep ranks of relevant items" - {
      val results = 1 to 100

      "when all relevant results are retrieved" in {
        ranks(results, Set(1, 10)) should be(Map("1" -> Option(1), "10" -> Option(10)))
      }

      "when one relevant result is missing" in {
        ranks(results, Set(1, 999)) should be(Map("1" -> Option(1), "999" -> None))
      }
    }
  }

  def avep(results: Seq[Int], relevant: Set[Int]) =
    QueryStats(0, "", results.map(_.toString), relevant.map(_.toString), Duration.Zero).averagePrecision

  def recall10(results: Seq[Int], relevant: Set[Int]) =
    QueryStats(0, "", results.map(_.toString), relevant.map(_.toString), Duration.Zero).recallAt(10)

  def ranks(results: Seq[Int], relevant: Set[Int]) =
    QueryStats(0, "", results.map(_.toString), relevant.map(_.toString), Duration.Zero).relRanks
}

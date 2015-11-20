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
  }

  def avep(results: Seq[Int], relevant: Set[Int]) =
    QueryStats[Int](0, "", results, relevant, Duration.Zero).averagePrecision

  def recall10(results: Seq[Int], relevant: Set[Int]) =
    QueryStats[Int](0, "", results, relevant, Duration.Zero).recallAt10
}

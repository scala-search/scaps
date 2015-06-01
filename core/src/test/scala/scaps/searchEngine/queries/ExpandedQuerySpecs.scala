package scaps.searchEngine.queries

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.webapi._

class ExpandedQuerySpecs extends FlatSpec with Matchers {
  import ExpandedQuery._

  def leaf(tpeName: String) =
    Leaf(new TypeEntity.PrimitiveType(tpeName)(Covariant), 0, 0)

  val A = leaf("A")
  val B = leaf("B")
  val C = leaf("C")

  val Box = leaf("Box")
  val Sox = leaf("Sox")

  "the expanded query minification" should ""

  ignore should "factor out redundant sub queries" in {
    // (Box & (A | B)) | (Sox & (A | B))
    // (Box | Sox) & (A | B)
    val q =
      Max(
        Sum(
          Box,
          Max(A, B)),
        Sum(
          Sox,
          Max(A, B)))

    ExpandedQuery.minimizeClauses(q) should be(
      Max(
        Sum(
          Max(Box, Sox),
          Max(A, B))))
  }
}

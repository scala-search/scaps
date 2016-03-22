/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.querying

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scaps.nucleus.Covariant
import scaps.nucleus.indexing.InternalTypes

class ExpandedQuerySpecs extends FlatSpec with Matchers {
  import ExpandedQuery._

  def leaf(tpeName: String) =
    Leaf(Covariant, tpeName, 1, 0, 0)

  val A = leaf("A")
  val B = leaf("B")
  val C = leaf("C")

  val Ax = leaf("Ax")
  val Bx = leaf("Bx")
  val Cx = leaf("Cx")
  val Dx = leaf("Dx")

  "the expanded query minification" should "rewrite single alternatives and parts" in {
    val q = Max(Sum(A), Sum(B))

    ExpandedQuery.minimize(q) should be(Max(A, B))

    val q2 = Max(Sum(Max(A), Max(B)))

    ExpandedQuery.minimize(q2) should be(Max(Sum(A, B)))
  }

  it should "factor out redundant sub queries" in {
    // (Ax & (A | B)) | (Bx & (A | B))
    // (Ax | Bx) & (A | B)
    val q =
      Max(
        Sum(
          Ax,
          Max(A, B)),
        Sum(
          Bx,
          Max(A, B)))

    ExpandedQuery.minimize(q) should be(
      Max(
        Sum(
          Max(Ax, Bx),
          Max(A, B))))
  }

  it should "factor out multiple redundant subqueries" in {
    // (Ax & A) | (Bx & A) | (Cx & B) | (Dx & B)
    // ((Ax | Bx) & A) | ((Cx | Dx) & B)
    val q =
      Max(
        Sum(Ax, A),
        Sum(Bx, A),
        Sum(Cx, B),
        Sum(Dx, B))

    ExpandedQuery.minimize(q) should be(
      Max(
        Sum(
          Max(Ax, Bx),
          A),
        Sum(
          Max(Cx, Dx),
          B)))
  }

  it should "handle repeated types correctly" in {
    // A & ((A & A) | B)
    val q = Sum(A, Max(Sum(A, A), B))

    ExpandedQuery.minimize(q) should be(q)
  }

  it should "handle common subqueries" in {
    // (Ax & A & A) | (Bx & A & A)
    val q = Max(Sum(Ax, A, A), Sum(Bx, A, A))

    ExpandedQuery.minimize(q) should be(
      Max(Sum(Max(Sum(Max(Ax, Bx), A)), A)))
  }
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.querying

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.nucleus.Covariant
import scaps.nucleus.indexing.FingerprintTerm

class QueryExpressionSpecs extends FlatSpec with Matchers {
  import QueryExpression._

  def fp(s: String) =
    s.split(" ").map(FingerprintTerm(_)).toList

  def p(unmatched: Int = 0, unmatchedOpt: Int = 0, unevaluated: Float = 0) =
    unmatched * 1f + unmatchedOpt * 0.1f + unevaluated

  implicit class TupleOps[A, B, C](x: (A, B, C)) {
    def pair = (x._1, x._2)
  }

  "a fingerprint scorer" should "score a simple type query" in {
    val scorer = Leaf("+A", 1, 0)

    scorer.score(fp("!+A")).pair should be((1, p()))
    scorer.score(fp("!+A !+B")).pair should be((1, p(unmatched = 1)))
    scorer.score(fp("!+B")).pair should be((0, p(unmatched = 1, unevaluated = 1)))
  }

  it should "score sum queries" in {
    val scorer =
      Sum(Leaf("+A", 1, 0) :: Leaf("+B", 2, 0) :: Nil)

    scorer.score(fp("!+A")).pair should be((1, p(unevaluated = 1)))
    scorer.score(fp("!+A !+B")).pair should be((3, p()))
  }

  it should "score max queries" in {
    val scorer =
      Max(Leaf("+A", 1, 0) :: Leaf("+B", 2, 0) :: Nil)

    scorer.score(fp("!+A")).pair should be((1, p()))
    scorer.score(fp("!+B")).pair should be((2, p()))
    scorer.score(fp("!+A !+B")).pair should be((2, p(unmatched = 1)))
    scorer.score(fp("!+B !+A")).pair should be((2, p(unmatched = 1)))
  }

  it should "allow multiple matches on sum queries that are part of a max query" in {
    val scorer =
      Max(
        Leaf("+A", 1, 0) ::
          Sum(
            Leaf("+B", 2, 0) ::
              Leaf("+C", 0.5f, 0) :: Nil) :: Nil)

    scorer.score(fp("!+A !+B !+C")).pair should be((2.5f, p(unmatched = 1)))
  }

  it should "score repeated types" in {
    val scorer =
      Sum(
        Max(
          Leaf("+A", 0.9f, 0) ::
            Leaf("+B", 0.5f, 0) :: Nil) ::
          Leaf("+A", 1, 0) ::
          Leaf("+A", 1, 0) :: Nil)

    scorer.score(fp("!+A !+A")).pair should be((2f, p(unevaluated = 1)))
    scorer.score(fp("!+A !+B")).pair should be((1.5f, p(unevaluated = 1)))
    scorer.score(fp("!+A !+A !+A")).pair should be((2.9f, p()))
    scorer.score(fp("!+B !+A !+A")).pair should be((2.5f, p()))
  }

  it should "score repeated types with varying values" in {
    val scorer =
      Sum(
        Max(
          Leaf("+A", 0.3f, 0) ::
            Leaf("+B", 0.5f, 0) :: Nil) ::
          Leaf("+A", 1, 0) :: Nil)

    scorer.score(fp("!+A !+B")).pair should be((1.5f, p()))
    scorer.score(fp("!+A !+A !+B")).pair should be((1.5f, p(unmatched = 1)))
  }

  it should "not penalize unmatched optional terms" in {
    val scorer = Leaf("+A", 1, 0)

    scorer.score(fp("!+A ?+A ?+B")).pair should be((1f, p(unmatchedOpt = 2)))
    scorer.score(fp("?+A ?+B")).pair should be((1f, p(unmatchedOpt = 1)))
    scorer.score(fp("?+B")).pair should be((0f, p(unmatchedOpt = 1, unevaluated = 1)))
  }

  // not relevant because up to now such expression trees do not exists
  ignore should "score sums with a total higher match" in {
    val scorer =
      Max(
        Leaf("+A", 1.5f, 0) ::
          Sum(
            Leaf("+B", 1, 0) ::
              Leaf("+C", 1, 0) :: Nil) :: Nil)

    scorer.score(fp("!+A !+B !+C"))._1 should be(2)
  }

  "the term frequency cutoff" should "not be exceeded" in {
    val terms =
      Max(Leaf("+A", 1, 0.5f) :: Leaf("+B", 0.9f, 0.4f) :: Leaf("+C", 0.8f, 0.3f) :: Nil)
        .termsBelowCutoff(1)

    terms should (
      contain("+A") and
      contain("+B") and
      not contain ("+C"))
  }

  it should "fill the quota with values with a lower score" in {
    val values =
      Max(Leaf("+A", 1, 0.5f) :: Leaf("+B", 0.9f, 0.6f) :: Leaf("+C", 0.8f, 0.3f) :: Nil)
        .termsBelowCutoff(1)

    values should (
      contain("+A") and
      contain("+C") and
      not contain ("+B"))
  }
}

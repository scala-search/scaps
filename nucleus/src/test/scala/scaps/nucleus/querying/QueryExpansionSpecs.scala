/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.querying

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.nucleus.Type
import scaps.nucleus.Definition
import scaps.nucleus.TestIndexAccess
import scaps.nucleus.indexing.InternalTypes
import scaps.nucleus.indexing.TypeViewIndex
import scaps.nucleus.indexing.TypeView
import scaps.nucleus.Contravariant
import scaps.nucleus.Covariant
import scaps.nucleus.Invariant

class QueryExpansionSpecs extends FlatSpec with Matchers {
  import scaps.nucleus.TestLanguage._
  import scaps.nucleus.querying.ExpandedQuery._
  import scaps.nucleus.indexing.{ InternalTypes => I }

  private[nucleus] def expand(views: TypeView*)(tpe: Type): ExpandedQuery = {
    val internal = I.toInternal(tpe, testModel)

    QueryExpansion.expandQuery(internal,
      t => TypeView(t.params, t.ref, t.ref) +: views)
  }

  it should "split a simple query into its parts" in {
    expand()(Type(Nil, +T.Fn(-T.Int, -T.Long, +T.String))) should be(
      Sum(
        Leaf(Contravariant, T.Int.name, 1d / 3, 0, 0),
        Leaf(Contravariant, T.Long.name, 1d / 3, 0, 0),
        Leaf(Covariant, T.String.name, 1d / 3, 0, 0)))
  }

  it should "use alternative types" in {
    expand(
      TypeView(Nil, +T.Char, +T.Int),
      TypeView(Nil, +T.Char, +T.Long))(
        Type(Nil, +T.Char)) should be(
          Sum(
            Max(
              Leaf(Covariant, T.Char.name, 1, 0, 0),
              Leaf(Covariant, T.Int.name, 1, 0, 1),
              Leaf(Covariant, T.Long.name, 1, 0, 1))))
  }

  it should "substitute type params" in {
    expand()(Type(tp("A") :: Nil, +A)) should be(
      Leaf(Covariant, I.Bottom().name, 1, 0, 0))
  }

  it should "split types with args into a sum query" in {
    expand()(Type(tp("A") :: Nil, +T.List(+A))) should be(
      Sum(
        Leaf(Covariant, T.List.name, 1d / 2, 0, 0),
        Leaf(Covariant, I.Bottom().name, 1d / 2, 1, 0)))
  }

  it should "handle alternatives with additional args" in {
    expand(
      TypeView(Nil, -T.BitSet, -T.Set(~T.Int)))(
        Type(Nil, -T.BitSet)) should be(
          Sum(
            Max(
              Leaf(Contravariant, T.BitSet.name, 1, 0, 0),
              Sum(
                Leaf(Contravariant, T.Set.name, 1d / 2, 0, 1),
                Leaf(Invariant, T.Int.name, 1d / 2, 1, 0)))))
  }

  it should "handle alternatives with unrelated args" in {
    expand(
      TypeView(tp("A") :: Nil, -T.List(-A), -T.Set(-T.Int)))(
        Type(tp("A") :: Nil, -T.List(-A))) should be(
          Sum(
            Max(
              Sum(
                Leaf(Contravariant, T.List.name, 1d / 2, 0, 0),
                Leaf(Contravariant, I.Top().name, 1d / 2, 1, 0)),
              Sum(
                Leaf(Contravariant, T.Set.name, 1d / 4, 0, 1),
                Leaf(Contravariant, T.Int.name, 1d / 4, 1, 0)))))
  }

  it should "expand nested types" in {
    expand(
      TypeView(tp("A") :: Nil, -T.List(-A), -T.Seq(-A)))(
        Type(tp("A") :: Nil, -T.List(-T.List(-A)))) should be(
          Sum(
            Max(
              Leaf(Contravariant, T.List.name, 1d / 2, 0, 0),
              Leaf(Contravariant, T.Seq.name, 1d / 2, 0, 1)),
            Max(
              Sum(
                Max(
                  Leaf(Contravariant, T.List.name, 1d / 4, 1, 0),
                  Leaf(Contravariant, T.Seq.name, 1d / 4, 1, 1)),
                Leaf(Contravariant, I.Top().name, 1d / 4, 2, 0)))))
  }
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.indexing

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.nucleus.Definition
import scaps.nucleus.IndexAccess
import scaps.nucleus.Type
import scaps.nucleus.TypeRef
import scaps.nucleus.indexing.InternalTypes.UnaryType
import scaps.nucleus.TestIndexAccess

class TypeViewIndexSpecs extends FlatSpec with Matchers {
  import scaps.nucleus.TestLanguage._
  import scaps.nucleus.indexing.{ InternalTypes => I }

  def alternatives(defs: Definition*)(tpe: Type): Seq[TypeRef] = {
    val index = new TestIndexAccess(defs)
    TypeViewIndex.typesViewableFrom(tpe, index)
  }

  it should "retrieve alternatives through elementary views" in {
    val alts = alternatives(extendss(+T.Int))_

    alts(Type(Nil, +T.Int)) should contain theSameElementsAs
      List(+T.Int, ~T.Int, +I.Bottom(), ~I.Unknown())

    alts(Type(Nil, -T.Int)) should contain theSameElementsAs
      List(-T.Int, ~T.Int, -I.Top(), ~I.Unknown())

    alts(Type(Nil, ~T.Int)) should contain theSameElementsAs
      List(~T.Int, ~I.Unknown())
  }

  it should "retrieve alternatives through additional views" in {
    alternatives(
      extendss(+T.Char, +T.Long),
      extendss(+T.Int, +T.Long))(
        Type(Nil, +T.Long)) should contain theSameElementsAs
        List(+T.Long, ~T.Long, +T.Int, +T.Char, +I.Bottom(), ~I.Unknown())

    alternatives(
      extendss(+T.Char, +T.Int, +T.Long))(
        Type(Nil, -T.Char)) should contain theSameElementsAs
        List(-T.Char, ~T.Char, -T.Int, -T.Long, -I.Top(), ~I.Unknown())

    alternatives(
      extendss(+T.Char, +T.Int, +T.Long))(
        Type(Nil, ~T.Char)) should contain theSameElementsAs
        List(~T.Char, ~I.Unknown())
  }

  it should "retrieve alternatives for generic types" in {
    val alts = alternatives(
      extendss(tp("A"))(+T.List(+A), +T.Seq(+A)))_

    alts(Type(tp("B") :: Nil, +T.Seq(+B))) should contain theSameElementsAs
      List(+T.Seq(+B), ~T.Seq(~B), +T.List(+B), +I.Bottom(+B), +I.Bottom(), ~I.Unknown())

    alts(Type(Nil, +T.Seq(+T.Int))) should contain theSameElementsAs
      List(+T.Seq(+T.Int), ~T.Seq(~T.Int), +T.List(+T.Int), +I.Bottom(+T.Int), +I.Bottom(), ~I.Unknown())
  }

  it should "alternatives with fewer arguments" in {
    val alts = alternatives(
      extendss(tp("A"))(+T.BitSet, +T.Set(+T.Int)))_

    alts(Type(Nil, +T.Set(+T.Int))) should contain(+T.BitSet)
    alts(Type(tp("B") :: Nil, +T.List(+B))) should not contain (+T.BitSet)
  }

  it should "alternatives with more arguments" in {
    val alts = alternatives(
      extendss(tp("A"))(+T.BitSet, +T.Set(+T.Int)))_

    alts(Type(Nil, -T.BitSet)) should contain(-T.Set(-T.Int))
  }

  it should "handle recursive types" in {
    object Loop extends InternalTypes.UnaryType("Loop")

    val alts = alternatives(
      extendss(tp("A"))(+Loop(+A), +T.List(+Loop(+A))))_

    alts(Type(tp("B") :: Nil, -Loop(-B))) should contain(-T.List(-Loop(-B)))
    alts(Type(tp("B") :: Nil, +T.List(+Loop(+B)))) should contain(+Loop(+B))
    alts(Type(Nil, +T.List(+Loop(+T.Int)))) should contain(+Loop(+T.Int))
    alts(Type(tp("B") :: Nil, +T.List(+B))).map(_.name) should not contain (Loop.name)
  }
}

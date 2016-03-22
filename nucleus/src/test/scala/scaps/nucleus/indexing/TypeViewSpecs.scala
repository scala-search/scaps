/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.indexing

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.nucleus.TypeDef
import scaps.nucleus.TypeRef
import scaps.nucleus.IndexSettings
import scaps.nucleus.Definition
import scaps.nucleus.TypeParam

private[nucleus] class TypeViewSpecs extends FlatSpec with Matchers {
  import scaps.nucleus.TestLanguage._
  import scaps.nucleus.indexing.{ InternalTypes => I }

  def typeViews(d: Definition) =
    TypeView.typeViews(I.toInternal(d, testModel))

  def v(from: TypeRef, to: TypeRef) =
    TypeView(Nil, I.toInternal(from, testModel), I.toInternal(to, testModel))

  def v(params: List[TypeParam], from: TypeRef, to: TypeRef) =
    TypeView(params, I.toInternal(from, testModel), I.toInternal(to, testModel))

  def assertViews(defsWithExpectedViews: (Definition, List[TypeView])*) = {
    defsWithExpectedViews.foreach {
      case (d, views) =>
        typeViews(d) should contain theSameElementsAs (views)
    }
  }

  "type view creation" should "create views to extermal types" in {
    assertViews(
      extendss(+T.Int) -> List())
  }

  it should "create views from simple subtype relations" in {
    assertViews(
      extendss(+T.Int, +T.Long) -> List(
        v(-T.Int, -T.Long),
        v(+T.Long, +T.Int)))
  }

  it should "create views from parametrized subtype relations" in {
    assertViews(
      extendss(tp("A"))(+T.List(+A), +T.Seq(+A)) -> List(
        v(tp("A") :: Nil, -T.List(-A), -T.Seq(-A)),
        v(tp("A") :: Nil, +T.Seq(+A), +T.List(+A))))
  }

  it should "create views from implicit conversion methods" in {
    assertViews(
      deff(-T.Int)(+T.Long, isImplicit = true) -> List(
        v(-T.Int, -T.Long),
        v(+T.Long, +T.Int)),
      vall(+T.Fn(-T.Int, +T.Long), isImplicit = true) -> List(
        v(-T.Int, -T.Long),
        v(+T.Long, +T.Int)))
  }

  it should "not create views to top types (these are elementary)" in {
    assertViews(
      deff(-T.Int)(+T.Any) -> List(),
      extendss()(+T.Int, +T.Any) -> List())
  }

  "type view application" should "apply a type to a view" in {
    v(-T.Int, -T.Long)
      .apply(-T.Int) should be(Some(-T.Long))
    v(-T.Int, -T.Long)
      .apply(-T.Long) should be(None)

    v(tp("A") :: Nil, -T.List(-A), -T.Seq(-A))
      .apply(-T.List(-T.Int)) should be(Some(-T.Seq(-T.Int)))

    v(tp("A") :: Nil, -T.List(-A), -T.Seq(-A))
      .apply(-T.List(-T.List(-B))) should be(Some(-T.Seq(-T.List(-B))))
  }
}

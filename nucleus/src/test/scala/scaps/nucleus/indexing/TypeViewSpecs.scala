package scaps.nucleus.indexing

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.nucleus.DefBuilder._
import scaps.nucleus.TestLanguage._
import scaps.nucleus.TypeDef
import scaps.nucleus.TypeRef
import scaps.nucleus.IndexSettings
import scaps.nucleus.Definition

private[nucleus] class TypeViewSpecs extends FlatSpec with Matchers {
  import scaps.nucleus.indexing.{ InternalTypes => I }

  def typeViews(d: Definition) =
    TypeView.typeViews(I.toInternal(d, testModel))

  def v(from: TypeRef, to: TypeRef) =
    TypeView(I.toInternal(from, testModel), I.toInternal(to, testModel))

  def assertViews(defsWithExpectedViews: (Definition, List[TypeView])*) = {
    defsWithExpectedViews.foreach {
      case (d, views) =>
        println(I.toInternal(d, testModel))
        typeViews(d) should contain theSameElementsAs (views)
    }
  }

  it should "create views to extermal types" in {
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
        v(-T.List(-I.__()), -T.Seq(-I.__())),
        v(+T.Seq(+I.__()), +T.List(+I.__()))))
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
}

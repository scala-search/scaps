package scaps.nucleus.indexing

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.nucleus.Definition
import scaps.nucleus.IndexAccess
import scaps.nucleus.Type
import scaps.nucleus.TypeRef

class TypeViewIndexSpecs extends FlatSpec with Matchers {
  import scaps.nucleus.TestLanguage._
  import scaps.nucleus.indexing.{ InternalTypes => I }

  def index(defs: Definition*): IndexAccess = {
    val docs = defs.flatMap(Indexer.defToDocs(_, testModel))
    println(docs.mkString("\n"))

    new IndexAccess {
      override def getByKeys(keys: Seq[String]) = {
        println(keys)
        docs.filter(d => keys.forall(d.keys.contains(_)))
      }
    }
  }

  def alternatives(index: IndexAccess, tpe: Type): Seq[TypeRef] = {
    TypeViewIndex.typesViewableFrom(tpe, index)
  }

  it should "retrieve alternatives through elementary views" in {
    alternatives(
      index(extendss(+T.Int)),
      Type(Nil, +T.Int)) should contain theSameElementsAs
      List(+T.Int, ~T.Int, +I.Bottom(), ~I.Unknown())
  }

  it should "retrieve alternatives through additional views" in {
    alternatives(
      index(
        extendss(+T.Char, +T.Long),
        extendss(+T.Int, +T.Long)),
      Type(Nil, +T.Long)) should contain theSameElementsAs
      List(+T.Long, ~T.Long, +T.Int, +T.Char, +I.Bottom(), ~I.Unknown())

    alternatives(
      index(
        extendss(+T.Char, +T.Int, +T.Long)),
      Type(Nil, -T.Char)) should contain theSameElementsAs
      List(-T.Char, ~T.Char, -T.Int, -T.Long, -I.Top(), ~I.Unknown())

    alternatives(
      index(
        extendss(+T.Char, +T.Int, +T.Long)),
      Type(Nil, ~T.Char)) should contain theSameElementsAs
      List(~T.Char, ~I.Unknown())
  }

  it should "retrieve alternatives for generic types" in {
    val idx = index(
      extendss(tp("A"))(+T.List(+A), +T.Seq(+A)))

    alternatives(
      idx,
      Type(tp("B") :: Nil, +T.Seq(+B))) should contain theSameElementsAs
      List(+T.Seq(+B), ~T.Seq(~B), +T.List(+B), +I.Bottom(+B), +I.Bottom(), ~I.Unknown())

    alternatives(
      idx,
      Type(Nil, +T.Seq(+T.Int))) should contain theSameElementsAs
      List(+T.Seq(+T.Int), ~T.Seq(~T.Int), +T.List(+T.Int), +I.Bottom(+T.Int), +I.Bottom(), ~I.Unknown())
  }
}

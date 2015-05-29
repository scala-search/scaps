package scaps.searchEngine.index

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.webapi._
import scaps.searchEngine.View
import scaps.searchEngine.SubType
import org.apache.lucene.store.RAMDirectory

class ViewIndexSpecs extends FlatSpec with Matchers {
  /*
   * Mocked type hierarchies:
   *
   *        A          X          Box[+T]      Box[C]
   *        ^                       ^            ^
   *    /---|---\                   |            |
   *    B       C                MyBox[+T]      CBox
   *            ^
   *            |
   *            D
   */
  val A = new TypeEntity.PrimitiveType("A")
  val B = new TypeEntity.PrimitiveType("B")
  val C = new TypeEntity.PrimitiveType("C")
  val D = new TypeEntity.PrimitiveType("D")
  val X = new TypeEntity.PrimitiveType("X")

  val T = (v: Variance) => TypeEntity("T", v, Nil, isTypeParam = true)
  val Wildcard = (v: Variance) => TypeEntity("_", v, Nil, isTypeParam = true)
  val Box = new TypeEntity.GenericType("Box")
  val MyBox = new TypeEntity.GenericType("MyBox")
  val CBox = new TypeEntity.PrimitiveType("CBox")

  val views = {
    def isSubTypeOf(cls: Variance => TypeEntity, base: Variance => TypeEntity, dist: Int): View =
      SubType(cls(Covariant), base(Covariant), dist)

    List(
      isSubTypeOf(B(_), A(_), 1),
      isSubTypeOf(C(_), A(_), 1),
      isSubTypeOf(D(_), C(_), 1),
      isSubTypeOf(D(_), A(_), 2),
      isSubTypeOf(v => MyBox(T(v), v), v => Box(T(v), v), 1),
      isSubTypeOf(CBox(_), v => Box(C(v), v), 1))
  }

  val viewIndex = {
    val index = new ViewIndex(new RAMDirectory)
    index.addEntities(views)
    index
  }

  "the views index" should "retrieve subtypes of types at covariant positions" in {
    val subtypesOfA = viewIndex.findAlternativesWithDistance(A(Covariant)).get

    subtypesOfA should (
      have length (3) and
      contain((B(Covariant), 1)) and
      contain((C(Covariant), 1)) and
      contain((D(Covariant), 2)))
  }

  it should "retrieve basetypes of types at contravariant positions" in {
    val basetypesOfD = viewIndex.findAlternativesWithDistance(D(Contravariant)).get

    basetypesOfD should (
      have length (2) and
      contain((C(Contravariant), 1)) and
      contain((A(Contravariant), 2)))
  }

  it should "retrieve <unknown> as an alternative of types at invariant positions" in {
    val alternatives = viewIndex.findAlternativesWithDistance(B(Invariant)).get

    alternatives should (
      have length (1) and
      contain((TypeEntity.Unknown(Invariant), 1)))
  }

  it should "retrieve subtypes of parametric types" in {
    val subtypesOfBox = viewIndex.findAlternativesWithDistance(Box(T(Covariant), Covariant)).get

    subtypesOfBox should (
      have length (1) and
      contain((MyBox(T(Covariant), Covariant), 1)) and
      not contain ((CBox(Covariant), 1)))
  }

  it should "retrieve subtypes of parametric types with concrete arguments" in {
    val subtypesOfBoxOfB = viewIndex.findAlternativesWithDistance(Box(B(Covariant), Covariant)).get

    subtypesOfBoxOfB should (
      have length (1) and
      contain((MyBox(B(Covariant), Covariant), 1)))

    val subtypesOfBoxOfC = viewIndex.findAlternativesWithDistance(Box(C(Covariant), Covariant)).get

    subtypesOfBoxOfC should (
      have length (2) and
      contain((MyBox(C(Covariant), Covariant), 1)) and
      contain((CBox(Covariant), 1)))
  }

  it should "retrieve parametric basetypes" in {
    val basetypesOfCBox = viewIndex.findAlternativesWithDistance(CBox(Contravariant)).get

    basetypesOfCBox should (
      have length (1) and
      contain((Box(C(Contravariant), Contravariant), 1)))
  }

  it should "retrieve basetypes of parametric types" in {
    val basetypesOfMyBox = viewIndex.findAlternativesWithDistance(MyBox(T(Contravariant), Contravariant)).get

    basetypesOfMyBox should (
      have length (1) and
      contain((Box(T(Contravariant), Contravariant), 1)))
  }

  it should "retrieve basetypes of parametric types with concrete arguments" in {
    val basetypesOfMyBoxOfC = viewIndex.findAlternativesWithDistance(MyBox(C(Contravariant), Contravariant)).get

    basetypesOfMyBoxOfC should (
      have length (1) and
      contain((Box(C(Contravariant), Contravariant), 1)))
  }
}

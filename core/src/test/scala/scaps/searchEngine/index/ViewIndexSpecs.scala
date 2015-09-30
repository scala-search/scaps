package scaps.searchEngine.index

import org.apache.lucene.store.RAMDirectory
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatcherFactory3.produceMatcher
import scaps.api.Contravariant
import scaps.api.Covariant
import scaps.api.Invariant
import scaps.api.TypeRef
import scaps.api.Variance
import scaps.api.ViewDef
import scaps.featureExtraction.Scala

class ViewIndexSpecs extends FlatSpec with Matchers {
  /*
   * Mocked type hierarchies:
   *
   *        A          X          Box[+T]      Box[C]    Box[Loop[T]]   Box[+T]
   *        ^                       ^            ^            ^            ^
   *    /---|---\                   |            |            |            |
   *    B       C                MyBox[+T]      CBox       Loop[+T]    InvarBox[T]
   *            ^
   *            |
   *            D
   */
  val A = new TypeRef.PrimitiveType("A")
  val B = new TypeRef.PrimitiveType("B")
  val C = new TypeRef.PrimitiveType("C")
  val D = new TypeRef.PrimitiveType("D")
  val X = new TypeRef.PrimitiveType("X")

  val T = (v: Variance) => TypeRef("T", v, Nil, isTypeParam = true)
  val U = (v: Variance) => TypeRef("U", v, Nil, isTypeParam = true)
  val Wildcard = (v: Variance) => TypeRef("_", v, Nil, isTypeParam = true)
  val Box = new TypeRef.GenericType("Box")
  val MyBox = new TypeRef.GenericType("MyBox")
  val CBox = new TypeRef.PrimitiveType("CBox")
  val Loop = new TypeRef.GenericType("Loop")
  val InvarBox = new TypeRef.GenericType("InvarBox")

  val Nothing = TypeRef.Nothing

  val views = {
    def isSubTypeOf(cls: Variance => TypeRef, base: Variance => TypeRef, dist: Int) =
      ViewDef.bidirectional(base(Covariant), cls(Covariant), dist, "")

    Scala.builtinViews ++
      List(
        isSubTypeOf(B(_), A(_), 1),
        isSubTypeOf(C(_), A(_), 1),
        isSubTypeOf(D(_), C(_), 1),
        isSubTypeOf(D(_), A(_), 2),
        isSubTypeOf(v => MyBox(T(v), v), v => Box(T(v), v), 1),
        isSubTypeOf(CBox(_), v => Box(C(v), v), 1),
        isSubTypeOf(v => Loop(T(v), v), v => Box(Loop(T(v), v), v), 1),
        isSubTypeOf(v => InvarBox(T(Invariant), v), v => Box(T(v), v), 1)).flatten
  }

  val viewIndex = {
    val index = new ViewIndex(new RAMDirectory)
    index.addEntities(views)
    index
  }

  "the views index" should "retrieve subtypes of types at covariant positions" in {
    val subtypesOfA = viewIndex.findAlternativesWithDistance(A(Covariant)).get

    subtypesOfA should (
      have length (4) and
      contain((B(Covariant), 1)) and
      contain((C(Covariant), 1)) and
      contain((D(Covariant), 2)) and
      contain((Nothing(Covariant), 1)))
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
      contain((TypeRef.Unknown(Invariant), 1)))
  }

  it should "retrieve subtypes of parametric types" in {
    val subtypesOfBox = viewIndex.findAlternativesWithDistance(Box(U(Covariant), Covariant)).get

    subtypesOfBox should (
      have length (3) and
      contain((MyBox(U(Covariant), Covariant), 1)) and
      contain((InvarBox(U(Invariant), Covariant), 1)) and
      not contain ((CBox(Covariant), 1)) and
      contain((Nothing(Covariant), 1)))
  }

  it should "retrieve subtypes of parametric types with concrete arguments" in {
    val subtypesOfBoxOfB = viewIndex.findAlternativesWithDistance(Box(B(Covariant), Covariant)).get

    subtypesOfBoxOfB should (
      have length (3) and
      contain((MyBox(B(Covariant), Covariant), 1)) and
      contain((InvarBox(B(Invariant), Covariant), 1)) and
      contain((Nothing(Covariant), 1)))

    val subtypesOfBoxOfC = viewIndex.findAlternativesWithDistance(Box(C(Covariant), Covariant)).get

    subtypesOfBoxOfC should (
      have length (4) and
      contain((MyBox(C(Covariant), Covariant), 1)) and
      contain((CBox(Covariant), 1)) and
      contain((InvarBox(C(Invariant), Covariant), 1)) and
      contain((Nothing(Covariant), 1)))
  }

  it should "retrieve parametric basetypes" in {
    val basetypesOfCBox = viewIndex.findAlternativesWithDistance(CBox(Contravariant)).get

    basetypesOfCBox should (
      have length (1) and
      contain((Box(C(Contravariant), Contravariant), 1)))
  }

  it should "retrieve basetypes of parametric types" in {
    val basetypesOfMyBox = viewIndex.findAlternativesWithDistance(MyBox(U(Contravariant), Contravariant)).get

    basetypesOfMyBox should (
      have length (1) and
      contain((Box(U(Contravariant), Contravariant), 1)))
  }

  it should "retrieve basetypes of parametric types with concrete arguments" in {
    val basetypesOfMyBoxOfC = viewIndex.findAlternativesWithDistance(MyBox(C(Contravariant), Contravariant)).get

    basetypesOfMyBoxOfC should (
      have length (1) and
      contain((Box(C(Contravariant), Contravariant), 1)))
  }

  it should "substitute type args in nested types" in {
    val baseTypesOfLoopOfA = viewIndex.findAlternativesWithDistance(Loop(A(Contravariant), Contravariant)).get

    baseTypesOfLoopOfA should (
      have length (1) and
      contain((Box(Loop(A(Contravariant), Contravariant), Contravariant), 1)))
  }
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

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
import scaps.scala.featureExtraction.Scala
import scaps.api.Module

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
    def isSubTypeOf(cls: Variance => TypeRef, base: Variance => TypeRef) =
      ViewDef.bidirectional(base(Covariant), cls(Covariant), "")

    List(
      ViewDef(TypeRef("_", Covariant, Nil, true), TypeRef.Nothing(Covariant), ""),
      ViewDef(TypeRef("_", Invariant, Nil, true), TypeRef.Unknown(Invariant), "")) ++
      List(
        isSubTypeOf(B(_), A(_)),
        isSubTypeOf(C(_), A(_)),
        isSubTypeOf(D(_), C(_)),
        isSubTypeOf(v => MyBox(T(v), v), v => Box(T(v), v)),
        isSubTypeOf(CBox(_), v => Box(C(v), v)),
        isSubTypeOf(v => Loop(T(v), v), v => Box(Loop(T(v), v), v)),
        isSubTypeOf(v => InvarBox(T(Invariant), v), v => Box(T(v), v))).flatten
  }

  val viewIndex = {
    val index = new ViewIndex(new RAMDirectory)
    index.addEntities(views)
    index
  }

  "the views index" should "retrieve subtypes of types at covariant positions" in {
    val subtypesOfA = viewIndex.findAlternatives(A(Covariant), 3).get

    subtypesOfA should (
      have length (4) and
      contain((B(Covariant))) and
      contain((C(Covariant))) and
      contain((D(Covariant))) and
      contain((Nothing(Covariant))))
  }

  it should "retrieve basetypes of types at contravariant positions" in {
    val basetypesOfD = viewIndex.findAlternatives(D(Contravariant), 3).get

    basetypesOfD should (
      have length (2) and
      contain((C(Contravariant))) and
      contain((A(Contravariant))))
  }

  it should "retrieve <unknown> as an alternative of types at invariant positions" in {
    val alternatives = viewIndex.findAlternatives(B(Invariant), 3).get

    alternatives should (
      have length (1) and
      contain((TypeRef.Unknown(Invariant))))
  }

  it should "retrieve subtypes of parametric types" in {
    val subtypesOfBox = viewIndex.findAlternatives(Box(U(Covariant), Covariant), 3).get

    subtypesOfBox should (
      have length (3) and
      contain((MyBox(U(Covariant), Covariant))) and
      contain((InvarBox(U(Invariant), Covariant))) and
      not contain ((CBox(Covariant))) and
      contain((Nothing(Covariant))))
  }

  it should "retrieve subtypes of parametric types with concrete arguments" in {
    val subtypesOfBoxOfB = viewIndex.findAlternatives(Box(B(Covariant), Covariant), 3).get

    subtypesOfBoxOfB should (
      have length (3) and
      contain((MyBox(B(Covariant), Covariant))) and
      contain((InvarBox(B(Invariant), Covariant))) and
      contain((Nothing(Covariant))))

    val subtypesOfBoxOfC = viewIndex.findAlternatives(Box(C(Covariant), Covariant), 3).get

    subtypesOfBoxOfC should (
      have length (4) and
      contain((MyBox(C(Covariant), Covariant))) and
      contain((CBox(Covariant))) and
      contain((InvarBox(C(Invariant), Covariant))) and
      contain((Nothing(Covariant))))
  }

  it should "retrieve parametric basetypes" in {
    val basetypesOfCBox = viewIndex.findAlternatives(CBox(Contravariant), 3).get

    basetypesOfCBox should (
      have length (1) and
      contain((Box(C(Contravariant), Contravariant))))
  }

  it should "retrieve basetypes of parametric types" in {
    val basetypesOfMyBox = viewIndex.findAlternatives(MyBox(U(Contravariant), Contravariant), 3).get

    basetypesOfMyBox should (
      have length (1) and
      contain((Box(U(Contravariant), Contravariant))))
  }

  it should "retrieve basetypes of parametric types with concrete arguments" in {
    val basetypesOfMyBoxOfC = viewIndex.findAlternatives(MyBox(C(Contravariant), Contravariant), 3).get

    basetypesOfMyBoxOfC should (
      have length (1) and
      contain((Box(C(Contravariant), Contravariant))))
  }

  it should "substitute type args in nested types" in {
    val baseTypesOfLoopOfA = viewIndex.findAlternatives(Loop(A(Contravariant), Contravariant), 3).get

    baseTypesOfLoopOfA should (
      have length (1) and
      contain((Box(Loop(A(Contravariant), Contravariant), Contravariant))))
  }
}

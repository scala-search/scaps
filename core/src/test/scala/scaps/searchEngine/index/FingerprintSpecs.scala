/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine.index

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.scala.featureExtraction.ExtractionUtils
import scaps.api.TypeRef
import scaps.api.Covariant
import scaps.api.TypeRef
import scaps.api.ValueDef
import scaps.utils.dump

class FingerprintSpecs extends FlatSpec with Matchers with ExtractionUtils {
  def extractFingerprints(source: String)(entityHandlers: (String, Fingerprint => Unit)*): Unit = {
    val adapted = entityHandlers.map { case (name, handler) => (name, (v: ValueDef) => handler(Fingerprint(v))) }
    extractValues(source)(adapted: _*)
  }

  "a type fingerprint" should "contain a value's type, variance and occurrence number" in {
    extractFingerprints("""
      package p

      object O {
        val i = 1
      }
      """)(
      ("p.O.i", _.toString should be("!+scala.Int")))
  }

  it should "support repeated types" in {
    extractFingerprints("""
      package p

      object O {
        val i = (1, 2, 3)
      }
      """)(
      ("p.O.i", _.termsWithIsOpt.count(_ == (("+scala.Int", false))) should be(3)))
  }

  it should "normalize member access" in {
    val ts = extractAllValues("""
      package p

      class C {
        val i = 1
      }

      object O {
        def m(c: C) = 1
        val f = (c: C) => 1
      }
      """)

    val i = Fingerprint(ts.find(_.name == "p.C.i").get)
    val m = Fingerprint(ts.find(_.name == "p.O.m").get)
    val f = Fingerprint(ts.find(_.name == "p.O.f").get)

    i should equal(m)
    i should equal(f)
  }

  it should "normalize method invocations" in {
    val ts = extractAllValues("""
      package p

      class C {
        def m1(i: Int) = 1
      }

      object O {
        def m2(c: C, i: Int) = 1
        def m3(c: C)(i: Int) = 1
      }
      """)

    val m1 = Fingerprint(ts.find(_.name == "p.C.m1").get)
    val m2 = Fingerprint(ts.find(_.name == "p.O.m2").get)
    val m3 = Fingerprint(ts.find(_.name == "p.O.m3").get)

    m1 should equal(m2)
    m1 should equal(m3)
  }

  it should "handle variance correctly" in {
    extractFingerprints("""
      package p

      trait T[-A, +B]

      object O {
        val v: T[T[Int, Char], T[Float, String]] = ???
        def m(x: T[Int, Char]): T[Float, String] = ???
      }

      class C {
        def m(x: T[Int, Char]): T[Float, String] = ???
      }
      """)(
      ("p.O.v", _.toString should (
        include("!+scala.Int") and
        include("!-scala.Char") and
        include("!-scala.Float") and
        include("!+java.lang.String"))),
      ("p.O.m", _.toString should (
        include("!+scala.Int") and
        include("!-scala.Char") and
        include("!-scala.Float") and
        include("!+java.lang.String"))),
      ("p.C.m", _.toString should (
        include("!-p.C") and
        include("!+scala.Int") and
        include("!-scala.Char") and
        include("!-scala.Float") and
        include("!+java.lang.String"))))
  }

  it should "include type arguments" in {
    extractFingerprints("""
      package p

      object O {
        val is = List(1)
      }
      """)(
      ("p.O.is", _.toString should (
        include("!+scala.collection.immutable.List") and
        include("!+scala.Int"))))
  }

  it should "use upper type parameter bounds at contravariant positions" in {
    extractFingerprints("""
      package p

      trait T {
        def m[X <: scala.AnyVal](x: X): Unit
      }
      """)(
      ("p.T.m", _.toString should (
        include("!-scala.AnyVal") and
        not include ("X"))))
  }

  it should "use lower type parameter bounds at covariant positions" in {
    extractFingerprints("""
      package p

      trait T {
        def m[X >: scala.AnyVal]: X
      }
      """)(
      ("p.T.m", _.toString should (
        include("!+scala.AnyVal") and
        not include ("X"))))
  }

  it should "use top type for unbound type parameters at contravariant positions" in {
    extractFingerprints("""
      package p

      trait T {
        def m[A](x: A): Unit
      }
      """)(
      ("p.T.m", _.toString should (
        include("!-scala.Any"))))
  }

  it should "use bottom type for unbound type parameters at covariant positions" in {
    extractFingerprints("""
      package p

      trait T {
        def m[A]: A
      }
      """)(
      ("p.T.m", _.toString should (
        include("!+scala.Nothing"))))
  }

  it should "support higher kinded type parameters" in {
    extractFingerprints("""
      package p

      trait Tr[A]

      object O {
        def m[M[X]](x: M[Int]): M[String] = ???
      }
      """)(
      ("p.O.m", _.toString should (
        include("!-<top1>")
        and not include ("!-Any")
        and include("!+<bottom1>")
        and not include ("!+Nothing")
        and include("!java.lang.String"))))
  }

  it should "use variance of bounds of higher kinded type parameters" in {
    extractFingerprints("""
      package p

      trait Tr[+A]

      object O {
        def m[M[X] <: Tr[X]](x: M[Int]): M[Float] = ???
      }
      """)(
      ("p.O.m", _.toString should (
        include("!-p.Tr")
        and include("!+<bottom1>")
        and not include ("!+Nothing")
        and include("!-scala.Int")
        and include("!+scala.Float"))))
  }

  it should "substitute all type parameters" in {
    extractFingerprints("""
      package p

      trait Tr[X] {}

      object O {
        def m[Y, M[X] <: Tr[X]](x: M[Y]): M[Y] = ???
      }
      """)(
      ("p.O.m", _.toString should (
        not include ("Y"))))
  }

  it should "handle variance correctly when using type params" in {
    extractFingerprints("""
      package p

      class Cl[+A] {
        def m[B <: AnyVal, C >: Int](x: Cl[B]): Cl[C]
      }
      """)(
      ("p.Cl.m", _.toString should (
        include("!-scala.Any") and
        include("!-scala.AnyVal") and
        include("!+scala.Int"))))
  }

  it should "treat objects implementing a single trait as an instance of this trait" in {
    extractFingerprints("""
      package p

      trait T

      object O extends T
      """)(
      ("p.O", _.termsWithIsOpt should be(List(("+p.T", false)))))
  }

  it should "ignore byName types" in {
    extractFingerprints("""
      package p

      object O {
        def m(i: => Int) = 1
      }
      """)(
      ("p.O.m", _.toString() should (
        not include (TypeRef.ByName.name) and
        include("!-scala.Int"))))
  }

  it should "not ignore repeated types" in {
    extractFingerprints("""
      package p

      object O {
        def m(i: Int*) = 1
      }
      """)(
      ("p.O.m", _.toString() should (
        include(TypeRef.Repeated.name) and
        include("!-scala.Int"))))
  }

  it should "preserve function arguments in higher kinded functions" in {
    extractFingerprints("""
      package p

      object O {
        def m(f: Int => String): String = f(1)
      }
      """)(
      ("p.O.m", _.toString() should (
        include(TypeRef.Function.name(1)))))
  }

  it should "mark types of implicit parameters as optional" in {
    extractFingerprints("""
      package p

      object O {
        def m[X: Ordering](a: X): X = ???
        def n[X](a: X)(implicit o: Ordering[X], i: Int): X = ???
      }
      """)(
      ("p.O.m", _.toString should (
        include("?-scala.math.Ordering") and
        include("?<unknown>") and
        include("!-scala.Any") and
        include("!+scala.Nothing"))),
      ("p.O.n", _.toString should (
        include("?-scala.math.Ordering") and
        include("?<unknown>") and
        include("?-scala.Int") and
        include("!-scala.Any") and
        include("!+scala.Nothing"))))
  }

  it should "mark repeated types as optional" in {
    extractFingerprints("""
      package p

      object O {
        def b(is: Int*): Int = ???
      }
      """)(
      ("p.O.b", _.toString should (
        include("?-scala.<repeated>"))))
  }
}

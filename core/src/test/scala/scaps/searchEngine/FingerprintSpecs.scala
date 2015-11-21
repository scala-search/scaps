package scaps.searchEngine

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scaps.scala.featureExtraction.ExtractionUtils
import scaps.api.Covariant
import scaps.api.TypeRef
import scaps.api.Variance

class TypeFingerprintSpecs extends FlatSpec with Matchers with ExtractionUtils {
  "a type fingerprint" should "contain a value's type, variance and occurrence number" in {
    extractValues("""
      package p

      object O {
        val i = 1
      }
      """)(
      ("p.O.i", _.typeFingerprint should be(List(TypeRef.Int(Covariant).term.toString))))
  }

  it should "support repeated types" in {
    extractValues("""
      package p

      object O {
        val i = (1, 2, 3)
      }
      """)(
      ("p.O.i", _.typeFingerprint.count(_ == "+scala.Int") should be(3)))
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

    val i = ts.find(_.name == "p.C.i").get
    val m = ts.find(_.name == "p.O.m").get
    val f = ts.find(_.name == "p.O.f").get

    i.typeFingerprint should equal(m.typeFingerprint)
    i.typeFingerprint should equal(f.typeFingerprint)
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

    val m1 = ts.find(_.name == "p.C.m1").get
    val m2 = ts.find(_.name == "p.O.m2").get
    val m3 = ts.find(_.name == "p.O.m3").get

    m1.typeFingerprint should equal(m2.typeFingerprint)
    m1.typeFingerprint should equal(m3.typeFingerprint)
  }

  it should "handle variance correctly" in {
    extractValues("""
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
      ("p.O.v", _.typeFingerprint.toString should (
        include("+scala.Int") and
        include("-scala.Char") and
        include("-scala.Float") and
        include("+java.lang.String"))),
      ("p.O.m", _.typeFingerprint.toString should (
        include("+scala.Int") and
        include("-scala.Char") and
        include("-scala.Float") and
        include("+java.lang.String"))),
      ("p.C.m", _.typeFingerprint.toString should (
        include("-p.C") and
        include("+scala.Int") and
        include("-scala.Char") and
        include("-scala.Float") and
        include("+java.lang.String"))))
  }

  it should "include type arguments" in {
    extractValues("""
      package p

      object O {
        val is = List(1)
      }
      """)(
      ("p.O.is", _.typeFingerprint.toString should (
        include("+scala.collection.immutable.List") and
        include("+scala.Int"))))
  }

  it should "use upper type parameter bounds at contravariant positions" in {
    extractValues("""
      package p

      trait T {
        def m[A <: scala.AnyVal](x: A): Unit
      }
      """)(
      ("p.T.m", _.typeFingerprint.toString should (
        include("-scala.AnyVal") and
        not include ("-A"))))
  }

  it should "use lower type parameter bounds at covariant positions" in {
    extractValues("""
      package p

      trait T {
        def m[A >: scala.AnyVal]: A
      }
      """)(
      ("p.T.m", _.typeFingerprint.toString should (
        include("+scala.AnyVal") and
        not include ("+A"))))
  }

  it should "use top type for unbound type parameters at contravariant positions" in {
    extractValues("""
      package p

      trait T {
        def m[A](x: A): Unit
      }
      """)(
      ("p.T.m", _.typeFingerprint.toString should (
        include("-scala.Any"))))
  }

  it should "use bottom type for unbound type parameters at covariant positions" in {
    extractValues("""
      package p

      trait T {
        def m[A]: A
      }
      """)(
      ("p.T.m", _.typeFingerprint.toString should (
        include("+scala.Nothing"))))
  }

  it should "support higher kinded type parameters" in {
    extractValues("""
      package p

      trait Tr[A]

      object O {
        def m[M[X] <: Tr[X]](x: M[Int]): M[String] = ???
      }
      """)(
      ("p.O.m", _.typeFingerprint.toString should (
        include("-p.Tr")
        and include("+scala.Nothing")
        and include("java.lang.String"))))
  }

  it should "use variance of bounds of higher kinded type parameters" in {
    extractValues("""
      package p

      trait Tr[+A]

      object O {
        def m[M[X] <: Tr[X]](x: M[Int]): M[Float] = ???
      }
      """)(
      ("p.O.m", _.typeFingerprint.toString should (
        include("-scala.Int")
        and include("+scala.Float"))))
  }

  it should "substitute all type parameters" in {
    extractValues("""
      package p

      trait Tr[X] {}

      object O {
        def m[Y, M[X] <: Tr[X]](x: M[Y]): M[Y] = ???
      }
      """)(
      ("p.O.m", _.typeFingerprint.toString should (
        not include ("Y"))))
  }

  it should "handle variance correctly when using type params" in {
    extractValues("""
      package p

      class Cl[+A] {
        def m[B <: AnyVal, C >: Int](x: Cl[B]): Cl[C]
      }
      """)(
      ("p.Cl.m", _.typeFingerprint.toString should (
        include("-scala.Any") and
        include("-scala.AnyVal") and
        include("+scala.Int"))))
  }

  it should "treat objects implementing a single trait as an instance of this trait" in {
    extractValues("""
      package p

      trait T

      object O extends T
      """)(
      ("p.O", _.typeFingerprint should be(List("+p.T"))))
  }

  it should "ignore byName types" in {
    extractValues("""
      package p

      object O {
        def m(i: => Int) = 1
      }
      """)(
      ("p.O.m", _.typeFingerprint.toString() should (
        not include (TypeRef.ByName.name) and
        include("-scala.Int"))))
  }

  it should "not ignore repeated types" in {
    extractValues("""
      package p

      object O {
        def m(i: Int*) = 1
      }
      """)(
      ("p.O.m", _.typeFingerprint.toString() should (
        include(TypeRef.Repeated.name) and
        include("-scala.Int"))))
  }

  it should "preserve function arguments in higher kinded functions" in {
    extractValues("""
      package p

      object O {
        def m(f: Int => String): String = f(1)
      }
      """)(
      ("p.O.m", _.typeFingerprint.toString() should (
        include(TypeRef.Function.name(1)))))
  }
}

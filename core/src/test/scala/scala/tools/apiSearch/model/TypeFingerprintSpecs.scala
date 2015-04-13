package scala.tools.apiSearch.model

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.tools.apiSearch.featureExtraction.ExtractionUtils

class TypeFingerprintSpecs extends FlatSpec with Matchers with ExtractionUtils {
  "a type fingerprint" should "contain a term's type, variance and occurrence number" in {
    extractTerms("""
      package p

      object O {
        val i = 1
      }
      """)(
      ("p.O.i", _.fingerprint should be("+scala.Int_0")))
  }

  it should "increse occurrence numbers on repeated types" in {
    extractTerms("""
      package p

      object O {
        val i = (1, 2, 3)
      }
      """)(
      ("p.O.i", _.fingerprint should (
        include("+scala.Int_0") and
        include("+scala.Int_1") and
        include("+scala.Int_2"))))
  }

  it should "handle variance correctly" in {
    extractTerms("""
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
      ("p.O.v", _.fingerprint should (
        include("+scala.Int_0") and
        include("-scala.Char_0") and
        include("-scala.Float_0") and
        include("+java.lang.String_0"))),
      ("p.O.m", _.fingerprint should (
        include("+scala.Int_0") and
        include("-scala.Char_0") and
        include("-scala.Float_0") and
        include("+java.lang.String_0"))),
      ("p.C#m", _.fingerprint should (
        include("-p.C_0") and
        include("+scala.Int_0") and
        include("-scala.Char_0") and
        include("-scala.Float_0") and
        include("+java.lang.String_0"))))
  }

  it should "include type arguments" in {
    extractTerms("""
      package p

      object O {
        val is = List(1)
      }
      """)(
      ("p.O.is", _.fingerprint should (
        include("+scala.collection.immutable.List_0") and
        include("+scala.Int_0"))))
  }

  it should "use upper type parameter bounds at contravariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A <: scala.AnyVal](x: A): Unit
      }
      """)(
      ("p.T#m", _.fingerprint should (
        include("-scala.AnyVal_0") and
        not include ("-A_0"))))
  }

  it should "use lower type parameter bounds at covariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A >: scala.AnyVal]: A
      }
      """)(
      ("p.T#m", _.fingerprint should (
        include("+scala.AnyVal_0") and
        not include ("+A_0"))))
  }

  it should "use top type for unbound type parameters at contravariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A](x: A): Unit
      }
      """)(
      ("p.T#m", _.fingerprint should (
        include("-scala.Any_0"))))
  }

  it should "use bottom type for unbound type parameters at covariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A]: A
      }
      """)(
      ("p.T#m", _.fingerprint should (
        include("+scala.Nothing_0"))))
  }

  it should "support higher kinded type parameters" in {
    extractTerms("""
      package p

      trait Tr[X] {}

      object O {
        def m[M[X] <: Tr[X]](x: M[Int]): M[String] = ???
      }
      """)(
      ("p.O.m", _.fingerprint should (
        include("-p.Tr_0")
        and include("+scala.Nothing_0")
        and include("java.lang.String_0"))))
  }

  it should "substitute all type parameters" in {
    extractTerms("""
      package p

      trait Tr[X] {}

      object O {
        def m[Y, M[X] <: Tr[X]](x: M[Y]): M[Y] = ???
      }
      """)(
      ("p.O.m", _.fingerprint should (
        not include ("Y"))))
  }

  it should "handle variance correctly when using type params" in {
    extractTerms("""
      package p

      class Cl[+A] {
        def m[B <: AnyVal, C >: Int](x: Cl[B]): Cl[C]
      }
      """)(
      ("p.Cl#m", _.fingerprint should (
        include("-scala.Any_0") and
        include("-scala.AnyVal_0") and
        include("+scala.Int_0"))))
  }
}

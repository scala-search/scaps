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

  it should "use upper type parameter bounds at covariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A <: scala.AnyVal]: A
      }
      """)(
      ("p.T#m", _.fingerprint should (
        include("+scala.AnyVal_0") and
        not include ("+A_0"))))
  }

  it should "use upper type parameter bounds at contravariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A <: scala.AnyVal](x: A): Any
      }
      """)(
      ("p.T#m", _.fingerprint should (
        include("-scala.AnyVal_0") and
        not include ("-A_0"))))
  }

  it should "ignore unbound type parameters" in {
    extractTerms("""
      package p

      trait T {
        def m[A](x: A): A
      }
      """)(
      ("p.T#m", _.fingerprint should (
        not include ("+scala.Any_0") and
        not include ("-scala.Any_0") and
        not include ("+A_0") and
        not include ("-A_0"))))
  }
}

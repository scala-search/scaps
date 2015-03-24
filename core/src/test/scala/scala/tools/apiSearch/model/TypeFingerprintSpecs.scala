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
      ("p.O.i", _.fingerprint should be("+scala.Int_1")))
  }

  it should "include type arguments" in {
    extractTerms("""
      package p

      object O {
        val is = List(1)
      }
      """)(
      ("p.O.is", is => {
        is.fingerprint should include("+scala.collection.immutable.List_1")
        is.fingerprint should include("+scala.Int_1")
      }))
  }

  it should "use upper type parameter bounds at covariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A <: scala.AnyVal]: A
      }
      """)(
      ("p.T#m", m => {
        m.fingerprint should include("+scala.AnyVal_1")
        m.fingerprint should not include ("+A_1")
      }))
  }

  it should "use upper type parameter bounds at contravariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A <: scala.AnyVal](x: A): Any
      }
      """)(
      ("p.T#m", m => {
        m.fingerprint should include("-scala.AnyVal_1")
        m.fingerprint should not include ("-A_1")
      }))
  }

  it should "ignore unbound type parameters" in {
    extractTerms("""
      package p

      trait T {
        def m[A](x: A): A
      }
      """)(
      ("p.T#m", m => {
        m.fingerprint should not include ("+scala.Any_1")
        m.fingerprint should not include ("-scala.Any_1")
        m.fingerprint should not include ("+A_1")
        m.fingerprint should not include ("-A_1")
      }))
  }
}

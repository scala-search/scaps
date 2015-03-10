package scala.tools.apiSearch.model

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.tools.apiSearch.testUtils.ExtractionUtils

class TypeFingerprintSpecs extends FlatSpec with Matchers with ExtractionUtils {
  "a type fingerprint" should "contain a term's type and variance" in {
    extractTerms("""
      package p

      object O {
        val i = 1
      }
      """)(
      ("p.O.i", _.fingerprint should be("+scala.Int")))
  }

  it should "include type arguments" in {
    extractTerms("""
      package p

      object O {
        val is = List(1)
      }
      """)(
      ("p.O.is", _.fingerprint should be("+scala.collection.immutable.List +scala.Int")))
  }

  it should "use upper type parameter bounds at covariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A <: scala.AnyVal]: A
      }
      """)(
      ("p.T#m", m => {
        m.fingerprint should include("+scala.AnyVal")
        m.fingerprint should not include ("+A")
      }))
  }

  it should "use lower type parameter bounds at contravariant positions" in {
    extractTerms("""
      package p

      trait T {
        def m[A >: Null](x: A): Any
      }
      """)(
      ("p.T#m", m => {
        m.fingerprint should include("-scala.Null")
        m.fingerprint should not include ("+A")
      }))
  }
}

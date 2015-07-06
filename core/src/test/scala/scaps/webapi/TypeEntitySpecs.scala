package scaps.webapi

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.featureExtraction.ExtractionUtils

class TypeEntitySpecs extends FlatSpec with Matchers with ExtractionUtils {
  "type entity" should "count the number of non-implicit parameters of method types" in {
    extractTerms("""
      package p

      object O {
        val a = 1
        def m(a: Int) = ???
        def n(a: Int)(b: Long, c: String) = ???
        def o(implicit a: Int) = ???
        def p(a: Int, b: Long)(implicit c: String) = ???
      }
      """)(
      ("p.O.a", _.tpe.explicitParamsCount should be(0)),
      ("p.O.m", _.tpe.explicitParamsCount should be(1)),
      ("p.O.n", _.tpe.explicitParamsCount should be(3)),
      ("p.O.o", _.tpe.explicitParamsCount should be(0)),
      ("p.O.p", _.tpe.explicitParamsCount should be(2)))
  }

  it should "count the number of non-implicit parameters of member types" in {
    extractTerms("""
      package p

      class C {
        val a = 1
        def m(a: Int) = ???
        def n(a: Int)(b: Long, c: String) = ???
        def o(implicit a: Int) = ???
        def p(a: Int, b: Long)(implicit c: String) = ???
      }
      """)(
      ("p.C#a", _.tpe.explicitParamsCount should be(1)),
      ("p.C#m", _.tpe.explicitParamsCount should be(2)),
      ("p.C#n", _.tpe.explicitParamsCount should be(4)),
      ("p.C#o", _.tpe.explicitParamsCount should be(1)),
      ("p.C#p", _.tpe.explicitParamsCount should be(3)))
  }
}

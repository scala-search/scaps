/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.scala.featureExtraction

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ScalaSourceExtractorUseCasesSpecs extends FlatSpec with Matchers with ExtractionUtils {

  it should "create value defs for use cases" in {
    val values = extractAllValues("""
      package p

      class C {
        /**
         * Doc
         *
         * @usecase def f(a: Int, b: Int): Int
         */
        def f[T: Ordering](a: T, b: T): T = ???
      }
      """)

    val fs = values.filter(_.name == "p.C.f")

    fs.size should be(2)
    fs.map(_.tpe.toString) should (
      contain("+<memberAccess>[-p.C, +<methodInvocation2>[-T, -T, +<methodInvocation1>[-<implicit>[-scala.math.Ordering[T]], +T]]]") and
      contain("+<memberAccess>[-p.C, +<methodInvocation2>[-scala.Int, -scala.Int, +scala.Int]]"))
  }

  it should "create inherited use cases" in {
    val values = extractAllValues("""
      package p

      class C {
        /**
         * Doc
         *
         * @usecase def f(a: Int, b: Int): Int
         */
        def f[T: Ordering](a: T, b: T): T = ???
      }

      class D extends C
      """)

    val fs = values.filter(_.name == "p.D.f")

    fs.size should be(2)
    fs.map(_.tpe.toString) should (
      contain("+<memberAccess>[-p.D, +<methodInvocation2>[-T, -T, +<methodInvocation1>[-<implicit>[-scala.math.Ordering[T]], +T]]]") and
      contain("+<memberAccess>[-p.D, +<methodInvocation2>[-scala.Int, -scala.Int, +scala.Int]]"))
  }
}

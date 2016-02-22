/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.utils

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class SampleSeqOpsSpecs extends FlatSpec with Matchers {
  "sample" should "yield the same list when invoked with n >= list.length" in {
    val l = Seq(1, 2, 3)

    l.sample(l.length) should be(l)
  }

  it should "yield an empty seq on n == 0" in {
    Seq(1, 2, 3).sample(0) should be(Seq())
  }

  it should "yield a list with n elements" in {
    val l = Seq(1, 2, 3, 4, 5)

    for { n <- 0 to l.length } {
      l.sample(n).length should be(n)
    }
  }

  it should "take random samples" in {
    val l = Seq(1, 2, 3, 4, 5)

    var sampleWasDifferent = false

    for { _ <- 1 to 100 } {
      sampleWasDifferent |= (l.sample(3) != l.sample(3))
    }

    sampleWasDifferent should be(true)
  }

  it should "evenly distribute values" in {
    val l = Seq(1, 2, 3)

    val res = List.newBuilder[Int]

    for { _ <- 1 to 1000 } {
      res ++= l.sample(2)
    }

    val countPerVal = res.result().groupBy(identity).mapValues(_.length)

    countPerVal.foreach { i =>
      i._2 shouldBe (666 +- 60)
    }
  }
}

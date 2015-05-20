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
      println(l.sample(n))
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
}

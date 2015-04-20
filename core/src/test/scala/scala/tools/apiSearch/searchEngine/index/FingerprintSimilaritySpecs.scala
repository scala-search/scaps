package scala.tools.apiSearch.searchEngine.index

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.tools.apiSearch.settings.Settings
import org.apache.lucene.index.FieldInvertState

class FingerprintSimilaritySpecs extends FlatSpec with Matchers {
  val settings = Settings.fromApplicationConf
  val sim = new TermsIndex.FingerprintSimilarity(settings)

  "fingerprint similarity length norm" should "yield distinct values for small lengths" in {
    (1 to 20).foldLeft(Float.MaxValue) { (previousNorm, length) =>
      val state = new FieldInvertState("fingerprint", 0, length, 0, 0, 1)
      val norm = sim.decodeNormValue(sim.computeNorm(state))

      withClue(s"with length $length: ") { norm should be < (previousNorm) }

      norm
    }

    ()
  }

  it should "yield 1.0 for length 1" in {
    val state = new FieldInvertState("fingerprint", 0, 1, 0, 0, 1)
    val norm = sim.lengthNorm(state)
    norm shouldEqual (1.0f)
  }
}

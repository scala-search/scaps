package scaps.scala.featureExtraction

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.api.FileSource
import scaps.api.Source
import scaps.api.ValueDef

class ScalaSourceExtractorSourceSpecs extends FlatSpec with Matchers with ExtractionUtils {

  def assertPos(start: Int, end: Int, fileId: Int = 0)(v: ValueDef) = {
    assertDefaultArtifact(v, fileId)
    if (v.source.startPos.getOrElse(0) < start || v.source.endPos.getOrElse(Int.MaxValue) > end) {
      fail(s"Pos of $v (${v.source.startPos}, ${v.source.endPos}) is not between $start and $end")
    }
  }

  def assertNoPos(fileId: Int = 0)(v: ValueDef) = {
    assertDefaultArtifact(v, fileId)
    v.source.startPos should be(None)
    v.source.endPos should be(None)
  }

  def assertDefaultArtifact(v: ValueDef, fileId: Int) = {
    v.source.artifactPath should be(Some(s"f$fileId.scala"))
  }

  it should "extract source locations" in {
    extractValues("""
      package p

      object O {
        val v = 1
      }
      """)(
      ("p.O", assertPos(24, 60)),
      ("p.O.v", assertPos(43, 52)),
      ("p.O.equals", assertNoPos()))
  }

  it should "inherit positions" in {
    extractValues("""
      package p

      class A {
        def m = 1
        def n = 1
      }

      class B extends A {
        def n = 2
      }
      """)(
      ("p.A.m", assertPos(40, 51)),
      ("p.B.m", assertPos(40, 51)),
      ("p.B.n", assertPos(105, 122)),
      ("p.B.equals", assertNoPos()))
  }

  it should "inherit positions accross source files" in {
    extractValues("""
      package p

      class A {
        def m = 1
        def n = 1
      }
      """, """
      package p

      class B extends A {
        def n = 2
      }
      """)(
      ("p.A.m", assertPos(40, 51, 0)),
      ("p.B.m", assertPos(40, 51, 0)),
      ("p.B.n", assertPos(44, 61, 1)))
  }
}

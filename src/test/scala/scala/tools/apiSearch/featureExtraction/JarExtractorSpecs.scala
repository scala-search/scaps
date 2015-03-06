package scala.tools.apiSearch.featureExtraction

import scala.tools.apiSearch.utils.CompilerAccess
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class JarExtractorSpecs extends FlatSpec with Matchers with CompilerAccess {
  val extractor = new JarExtractor(compiler)
  val scalaLibSources = "/Applications/eclipseScala/plugins/org.scala-lang.scala-library.source_2.11.5.v20150101-184742-3fafbc204f.jar"

  "the jar extractor" should "work" in {
    println(extractor(scalaLibSources))
  }
}
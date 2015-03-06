package scala.tools.apiSearch.featureExtraction

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.tools.apiSearch.utils.CompilerAccess

class JarExtractorSpecs extends FlatSpec with Matchers with CompilerAccess {
  val extractor = new JarExtractor(compiler)

  "the jar extractor" should "extract entities from source files in jars" in {
    val path = getClass.getResource("/jarExtractorTests.jar").toURI().getPath
    val entities = extractor(path)
    
    val m = entities.find(e => e.name == "jarExtractorTests.O.m")
    
    m should be('defined)
  }
}
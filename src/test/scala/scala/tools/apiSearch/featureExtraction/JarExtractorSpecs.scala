package scala.tools.apiSearch.featureExtraction

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.tools.apiSearch.utils.CompilerAccess
import java.io.File

class JarExtractorSpecs extends FlatSpec with Matchers with CompilerAccess {
  val extractor = new JarExtractor(compiler)

  "the jar extractor" should "extract entities from source files in jars" in {
    val file = new File(getClass.getResource("/jarExtractorTests.jar").toURI().getPath)
    val entities = extractor(file)

    entities.find(_.name == "jarExtractorTests.C") should be('defined)
    entities.find(_.name == "jarExtractorTests.O.m") should be('defined)
  }
}

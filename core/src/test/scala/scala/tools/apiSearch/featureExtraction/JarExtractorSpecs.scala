package scala.tools.apiSearch.featureExtraction

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.tools.apiSearch.utils.CompilerAccess
import java.io.File

class JarExtractorSpecs extends FlatSpec with Matchers with CompilerAccess {
  val extractor = new JarExtractor(initCompiler())
  val extractorTestSources = new File(getClass.getResource("/jarExtractorTests.jar").toURI().getPath)

  "the jar extractor" should "extract entities from source files in jars" in {
    val entities = extractor(extractorTestSources)

    entities.find(_.name == "jarExtractorTests.C") should be('defined)
    entities.find(_.name == "jarExtractorTests.O.m") should be('defined)
  }

  it should "work with files with identical names" in {
    val entities = extractor(extractorTestSources)

    entities.find(_.name == "jarExtractorTests.C") should be('defined)
    entities.find(_.name == "jarExtractorTests.p.C") should be('defined)
  }
}

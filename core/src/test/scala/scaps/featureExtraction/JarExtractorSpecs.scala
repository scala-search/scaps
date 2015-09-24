package scaps.featureExtraction

import org.scalatest.Matchers
import org.scalatest.FlatSpec

import java.io.File

class JarExtractorSpecs extends FlatSpec with Matchers {

  "the jar extractor" should "extract entities from source files in jars" in {
    val entities = entitiesFromTestSources()

    entities.find(_.name == "jarExtractorTests.C") should be('defined)
    entities.find(_.name == "jarExtractorTests.O.m") should be('defined)
  }

  it should "work with files with identical names" in {
    val entities = entitiesFromTestSources()

    entities.find(_.name == "jarExtractorTests.C") should be('defined)
    entities.find(_.name == "jarExtractorTests.p.C") should be('defined)
  }

  def entitiesFromTestSources() = {
    val compiler = CompilerUtils.createCompiler(Nil)
    val extractor = new JarExtractor(compiler)
    val extractorTestSources = new File(getClass.getResource("/jarExtractorTests.jar").toURI().getPath)
    extractor(extractorTestSources).flatMap(_.fold(_ => Nil, List(_))).toList
  }
}

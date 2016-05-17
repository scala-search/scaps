/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.scala.featureExtraction

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import java.io.File
import scaps.api.ValueDef
import scaps.api.FileSource
import scaps.api.PosSource

class JarExtractorSpecs extends FlatSpec with Matchers {

  val testJarPath = getClass.getResource("/jarExtractorTests.jar").toURI().getPath

  val entities = entitiesFromTestSources()

  "the jar extractor" should "extract entities from source files in jars" in {
    entities.find(_.name == "jarExtractorTests.C") should be('defined)
    entities.find(_.name == "jarExtractorTests.O.m") should be('defined)
  }

  it should "work with files with identical names" in {
    entities.find(_.name == "jarExtractorTests.C") should be('defined)
    entities.find(_.name == "jarExtractorTests.p.C") should be('defined)
  }

  it should "create nested file sources" in {
    val m = entities.collectFirst {
      case v: ValueDef if v.name == "jarExtractorTests.O.m" => v
    }.get

    m.source should matchPattern {
      case FileSource(`testJarPath`, FileSource(innerPath, pos: PosSource)) if pos.start >= 38 && pos.end <= 49 => ()
    }
  }

  def entitiesFromTestSources() = {
    val compiler = CompilerUtils.createCompiler(Nil)
    val extractor = new JarExtractor(compiler)
    val extractorTestSources = new File(testJarPath)
    extractor(extractorTestSources).flatMap(_.fold(_ => Nil, List(_))).toList
  }
}

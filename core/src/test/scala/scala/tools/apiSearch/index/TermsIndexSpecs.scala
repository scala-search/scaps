package scala.tools.apiSearch.index

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.apache.lucene.store.RAMDirectory
import scala.tools.apiSearch.utils.using
import scala.tools.apiSearch.featureExtraction.ScalaSourceExtractor
import scala.tools.apiSearch.model._

class TermsIndexSpecs extends FlatSpec with Matchers with IndexUtils {
  "the index" should "persist entities and retrieve them by name" in {
    withTermIndex("""
      package p

      object O{
        val v = 1
      }
      """) { index =>
      index.findTermsByName("p.O.v").get should contain(TermEntity("p.O.v", Nil, TypeEntity.int, ""))
    }
  }
}

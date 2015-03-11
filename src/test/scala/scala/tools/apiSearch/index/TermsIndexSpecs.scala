package scala.tools.apiSearch.index

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.tools.apiSearch.testUtils.ExtractionUtils
import org.apache.lucene.store.RAMDirectory
import scala.tools.apiSearch.utils.using
import scala.tools.apiSearch.featureExtraction.ScalaSourceExtractor
import scala.tools.apiSearch.model._

class TermsIndexSpecs extends FlatSpec with Matchers with ExtractionUtils {
  "the index" should "persist entities and retrieve them by name" in {
    withTermIndex("""
      package p

      object O{
        val v = 1
      }
      """) { index =>
      index.findTermsByName("p.O.v").get should contain(TermEntity("p.O.v", Nil, TypeEntity("scala.Int", Covariant, Nil), ""))
    }
  }

  def withTermIndex(f: TermsIndex => Unit): Unit = {
    using(new RAMDirectory) { termsDir =>
      val index = new TermsIndex(termsDir)
      f(index)
    }
  }

  def withTermIndex(sources: String*)(f: TermsIndex => Unit): Unit = withTermIndex { index =>
    val entities = sources.toStream.flatMap(extractAll).collect { case t: TermEntity => t }
    index.addEntities(entities)
    f(index)
  }
}

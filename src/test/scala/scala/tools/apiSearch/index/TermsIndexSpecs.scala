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
    withIndex("""
      package p

      class C{
        def m: Int
      }
      """) { index =>
      index.findTermsByName("p.C#m").get.head.name should be("p.C#m")
    }
  }

  def withIndex(f: TermsIndex => Unit): Unit = {
    using(new RAMDirectory) { termsDir =>
      val index = new TermsIndex(termsDir)
      f(index)
    }
  }

  def withIndex(sources: String*)(f: TermsIndex => Unit): Unit = withIndex { index =>
    val entities = sources.toStream.flatMap(extractAll).collect { case t: TermEntity => t }
    index.addEntities(entities)
    f(index)
  }
}

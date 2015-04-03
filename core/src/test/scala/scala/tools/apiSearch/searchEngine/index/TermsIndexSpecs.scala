package scala.tools.apiSearch.searchEngine.index

import scala.tools.apiSearch.model.TermEntity
import scala.tools.apiSearch.model.TypeEntity

import org.scalatest.FlatSpec
import org.scalatest.Matchers

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

  it should "tokenize full qualified names" in {
    withTermIndex("""
      package pkg

      object Obj{
        val value = 1
      }
      """) { index =>
      val value = TermEntity("pkg.Obj.value", Nil, TypeEntity.int, "")
      index.findTermsByName("value").get should contain(value)
      index.findTermsByName("Obj").get should contain(value)
      index.findTermsByName("pkg").get should contain(value)
    }
  }

  it should "tokenize names on case changes" in {
    withTermIndex("""
      package somePkg

      object AnotherObj{
        val myValue = 1
      }
      """) { index =>
      val value = TermEntity("somePkg.AnotherObj.myValue", Nil, TypeEntity.int, "")
      index.findTermsByName("value").get should contain(value)
      index.findTermsByName("Another").get should contain(value)
      index.findTermsByName("pkg").get should contain(value)
    }
  }

  it should "ignore cases in names" in {
    withTermIndex("""
      package scala.tools.apiSearch.searchEngine.index

      object O{
        val vAlUe = 1
      }
      """) { index =>
      val value = TermEntity("p.O.vAlUe", Nil, TypeEntity.int, "")
      index.findTermsByName("value").get should contain(value)
      index.findTermsByName("VALUE").get should contain(value)
      index.findTermsByName("VALue").get should contain(value)
    }
  }
}

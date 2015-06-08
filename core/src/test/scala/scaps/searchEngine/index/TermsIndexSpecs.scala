package scaps.searchEngine.index

import scaps.webapi.TermEntity
import scaps.webapi.TypeEntity

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
      index.findTermsByName("p.o.v").get should (have size 1)
    }
  }

  it should "tokenize full qualified names" in {
    withTermIndex("""
      package pkg

      object Obj{
        val value = 1
      }
      """) { index =>
      val value = TermEntity("pkg.Obj.value", Nil, TypeEntity.Int(), "")
      index.findTermsByName("value").get should contain(value)
      index.findTermsByName("obj").get should contain(value)
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
      val value = TermEntity("somePkg.AnotherObj.myValue", Nil, TypeEntity.Int(), "")
      index.findTermsByName("value").get should contain(value)
      index.findTermsByName("another").get should contain(value)
      index.findTermsByName("pkg").get should contain(value)
    }
  }
}

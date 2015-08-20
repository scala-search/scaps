package scaps.searchEngine.index

import scaps.webapi.ValueDef
import scaps.webapi.TypeRef

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ValuesIndexSpecs extends FlatSpec with Matchers with IndexUtils {
  "the index" should "persist entities and retrieve them by name" in {
    withValueIndex("""
      package p

      object O{
        val v = 1
      }
      """) { index =>
      val v = ValueDef("p.O.v", Nil, TypeRef.Int(), "", Set(ValueDef.Static))
      index.findValuesByName("v").get should (have size 1)
    }
  }

  it should "tokenize full qualified names" in {
    withValueIndex("""
      package pkg

      object Obj{
        val value = 1
      }
      """) { index =>
      val value = ValueDef("pkg.Obj.value", Nil, TypeRef.Int(), "", Set(ValueDef.Static))
      index.findValuesByName("value").get should contain(value)
      index.findValuesByName("obj").get should contain(value)
      index.findValuesByName("pkg").get should contain(value)
      index.findValuesByName("Pkg Obj").get should contain(value)
    }
  }

  it should "tokenize names on case changes" in {
    withValueIndex("""
      package somePkg

      object AnotherObj{
        val myValue = 1
      }
      """) { index =>
      val value = ValueDef("somePkg.AnotherObj.myValue", Nil, TypeRef.Int(), "", Set(ValueDef.Static))
      index.findValuesByName("value").get should contain(value)
      index.findValuesByName("another").get should contain(value)
      index.findValuesByName("pkg").get should contain(value)
      index.findValuesByName("another Value").get should contain(value)
    }
  }

  it should "tokenize operators" in {
    withValueIndex("""
      package p

      object ::{
        val ++ = 1
      }
      """) { index =>
      val value = ValueDef("p.::.++", Nil, TypeRef.Int(), "", Set(ValueDef.Static))
      index.findValuesByName("::").get should contain(value)
      index.findValuesByName("++").get should contain(value)
    }
  }
}

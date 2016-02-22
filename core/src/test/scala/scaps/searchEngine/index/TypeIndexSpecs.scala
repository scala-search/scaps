/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine.index

import scaps.api._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TypeIndexSpecs extends FlatSpec with Matchers with IndexUtils {
  "the type index" should "persist class entities only once" in {
    withTypeIndex("""
      package p

      class C
      """) { index =>
      val C = cls("p.C")()()

      index.addEntities(C :: Nil)

      index.findTypeDefsBySuffix("C").get.size should be(1)
    }
  }

  it should "retrieve class entities by suffix" in {
    withTypeIndex("""
      package p.q

      class C
      """) { index =>
      val C = cls("p.q.C")()()

      index.findTypeDefsBySuffix("C").get should contain(C)
      index.findTypeDefsBySuffix("q.C").get should contain(C)
      index.findTypeDefsBySuffix("p.q.C").get should contain(C)
    }
  }

  it should "retrieve nested class entities by suffix" in {
    withTypeIndex("""
      package p.q

      class C{
        trait T
      }
      """) { index =>
      val T = cls("p.q.C.T")()()

      index.findTypeDefsBySuffix("T").get should contain(T)
      index.findTypeDefsBySuffix("C.T").get should contain(T)
      index.findTypeDefsBySuffix("q.C.T").get should contain(T)
      index.findTypeDefsBySuffix("p.q.C.T").get should contain(T)
    }
  }

  it should "retrieve multiple typeDefs with same suffix" in {
    withTypeIndex("""
      package p

      class C{
        trait T
      }

      class D{
        trait T
      }
      """) { index =>
      val CT = cls("p.C.T")()()
      val DT = cls("p.D.T")()()

      val result = index.findTypeDefsBySuffix("T").get

      result should contain allOf (CT, DT)
    }
  }

  def tpe(name: String, args: List[TypeRef] = Nil) =
    TypeRef(name, Covariant, args)

  def cls(name: String)(args: String*)(baseTypes: TypeRef*) =
    TypeDef(
      name,
      args.map(TypeParameter(_, Invariant)).toList)
}

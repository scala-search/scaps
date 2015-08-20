package scaps.searchEngine.index

import scaps.webapi._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ClassIndexSpecs extends FlatSpec with Matchers with IndexUtils {
  "the class index" should "persist class entities only once" in {
    withClassIndex("""
      package p

      class C
      """) { index =>
      val C = cls("p.C")()()

      index.addEntities(C :: Nil)

      index.findClassBySuffix("C").get.size should be(1)
    }
  }

  it should "retrieve class entities by suffix" in {
    withClassIndex("""
      package p.q

      class C
      """) { index =>
      val C = cls("p.q.C")()()

      index.findClassBySuffix("C").get should contain(C)
      index.findClassBySuffix("q.C").get should contain(C)
      index.findClassBySuffix("p.q.C").get should contain(C)
    }
  }

  it should "retrieve nested class entities by suffix" in {
    withClassIndex("""
      package p.q

      class C{
        trait T
      }
      """) { index =>
      val T = cls("p.q.C.T")()()

      index.findClassBySuffix("T").get should contain(T)
      index.findClassBySuffix("C.T").get should contain(T)
      index.findClassBySuffix("q.C.T").get should contain(T)
      index.findClassBySuffix("p.q.C.T").get should contain(T)
    }
  }

  it should "retrieve multiple classes with same suffix" in {
    withClassIndex("""
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

      val result = index.findClassBySuffix("T").get

      result should contain allOf (CT, DT)
    }
  }

  def tpe(name: String, args: List[TypeEntity] = Nil) =
    TypeEntity(name, Covariant, args)

  def cls(name: String)(args: String*)(baseTypes: TypeEntity*) =
    TypeDef(
      name,
      args.map(TypeParameterEntity(_, Invariant)).toList,
      baseTypes.toList ++ List(TypeEntity.AnyRef(), TypeEntity.Any()))
}

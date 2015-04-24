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
      val T = cls("p.q.C#T")()()

      index.findClassBySuffix("T").get should contain(T)
      index.findClassBySuffix("C#T").get should contain(T)
      index.findClassBySuffix("q.C#T").get should contain(T)
      index.findClassBySuffix("p.q.C#T").get should contain(T)
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
      val CT = cls("p.C#T")()()
      val DT = cls("p.D#T")()()

      val result = index.findClassBySuffix("T").get

      result should contain allOf (CT, DT)
    }
  }

  it should "retrieve subclasses" in {
    withClassIndex("""
      package p

      trait A

      trait B extends A
      """) { index =>
      val A = cls("p.A")()()
      val B = cls("p.B")()(TypeEntity(A.name))

      val result = index.findSubClasses(TypeEntity("p.A")).get

      result should (contain(B) and not contain (A))

      val anySubClasses = index.findSubClasses(TypeEntity.Any()).get

      anySubClasses should contain allOf (A, B)
    }
  }

  it should "consider type arguments when retrieving subclasses" in {
    withClassIndex("""
      package p

      trait A[T]

      trait B extends A[Int]
      trait C extends B
      trait D extends A[String]
      """) { index =>
      index.findSubClasses(TypeEntity("p.A", List(TypeEntity.Int()))).get.map(_.name) should (
        contain("p.B") and
        contain("p.C") and
        not contain ("p.D"))

      index.findSubClasses(TypeEntity("p.A", List(TypeEntity.String()))).get.map(_.name) should (
        not contain ("p.B") and
        not contain ("p.C") and
        contain("p.D"))
    }
  }

  def cls(name: String)(args: String*)(baseTypes: TypeEntity*) =
    ClassEntity(name, args.map(TypeParameterEntity(_, Invariant)).toList, baseTypes.toList ++ List(TypeEntity.AnyRef(), TypeEntity.Any()))
}

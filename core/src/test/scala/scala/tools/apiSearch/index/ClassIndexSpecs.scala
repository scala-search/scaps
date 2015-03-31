package scala.tools.apiSearch.index

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.tools.apiSearch.model._
import org.apache.lucene.store.RAMDirectory
import scala.tools.apiSearch.utils.using
import scala.tools.apiSearch.featureExtraction.ExtractionUtils

class ClassIndexSpecs extends FlatSpec with Matchers with IndexUtils {
  "the class index" should "persist class entities and retrieve them by name" in {
    withClassIndex("""
      package p

      class C
      """) { index =>
      val C = cls("p.C")()()

      index.findClassByName("p.C").get should be(Option(C))
    }
  }

  it should "persist class entities only once" in {
    withClassIndex("""
      package p

      class C
      """) { index =>
      val C = cls("p.C")()()

      index.addEntities(C :: Nil)

      index.findClass("C").get.size should be(1)
    }
  }

  it should "retrieve class entities by suffix" in {
    withClassIndex("""
      package p.q

      class C
      """) { index =>
      val C = cls("p.q.C")()()

      index.findClass("C").get should contain(C)
      index.findClass("q.C").get should contain(C)
      index.findClass("p.q.C").get should contain(C)
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

      index.findClass("T").get should contain(T)
      index.findClass("C#T").get should contain(T)
      index.findClass("q.C#T").get should contain(T)
      index.findClass("p.q.C#T").get should contain(T)
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

      val result = index.findClass("T").get

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

      val result = index.findSubClasses(A).get

      result should (contain(B) and not contain (A))

      val anySubClasses = index.findSubClasses(ClassEntity(TypeEntity.any.name, Nil, Nil)).get

      anySubClasses should contain allOf (A, B)
    }
  }

  def cls(name: String)(args: String*)(baseTypes: TypeEntity*) =
    ClassEntity(name, args.map(TypeParameterEntity(_, Invariant)).toList, baseTypes.toList ++ List(TypeEntity.anyRef, TypeEntity.any))
}

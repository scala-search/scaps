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

  it should "retrieve class entities by suffix" in {
    withClassIndex("""
      package p.q

      class C
      """) { index =>
      val C = cls("p.q.C")()()

      index.findClass("C", 0).get should contain(C)
      index.findClass("q.C", 0).get should contain(C)
      index.findClass("p.q.C", 0).get should contain(C)
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

      index.findClass("T", 0).get should contain(T)
      index.findClass("C#T", 0).get should contain(T)
      index.findClass("q.C#T", 0).get should contain(T)
      index.findClass("p.q.C#T", 0).get should contain(T)
    }
  }

  it should "retrieve multiple classes with same suffix and arity" in {
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

      val result = index.findClass("T", 0).get

      result should contain(CT)
      result should contain(DT)
    }
  }

  it should "retrieve only classes with given arity" in {
    withClassIndex("""
      package p

      object O1 {
        class C[A]
      }

      object O2 {
        class C[A, B]
      }
      """) { index =>
      val C1 = cls("p.O1.C")("A")()
      val C2 = cls("p.O2.C")("A", "B")()

      val result = index.findClass("C", 1).get

      result should contain(C1)
      result should not contain (C2)
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

      val result = index.findSubClasses(A.name).get

      result should contain(B)
      result should not contain (A)

      val anySubClasses = index.findSubClasses(TypeEntity.any.name).get

      anySubClasses should contain(A)
      anySubClasses should contain(B)
    }
  }

  def cls(name: String)(args: String*)(baseTypes: TypeEntity*) =
    ClassEntity(name, args.map(TypeParameterEntity(_, Invariant)).toList, baseTypes.toList ++ List(TypeEntity.anyRef, TypeEntity.any))
}

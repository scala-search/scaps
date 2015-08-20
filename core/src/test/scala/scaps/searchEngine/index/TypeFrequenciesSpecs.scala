package scaps.searchEngine.index

import org.scalatest.FlatSpec

import scaps.webapi.Contravariant
import scaps.webapi.Covariant
import scaps.webapi.Invariant
import scaps.webapi.ValueDef
import scaps.webapi.TypeRef
import scaps.webapi.View

class TypeFrequenciesSpecs extends FlatSpec with IndexUtils {
  "the type frequency accumulator" should "calculate type frequencies at covariant positions" in {
    val (tfs, maxFreq) = typeFrequenciesWithMaxAbsoluteFrequency("""
      package p

      trait A
      class B extends A
      class B2 extends A
      class C extends B

      object O {
        def m1: A = ???
        def m2: B = ???
        def m3: C = ???
      }
      """)

    val tfAny = tfs((Covariant, TypeRef.Any.name))
    val tfA = tfs((Covariant, "p.A"))
    val tfB = tfs((Covariant, "p.B"))
    val tfB2 = tfs((Covariant, "p.B2"))
    val tfC = tfs((Covariant, "p.C"))
    val tfNothing = tfs((Covariant, TypeRef.Nothing.name))

    tfAny should be(0f)
    tfA should be(1f / maxFreq) // m1
    tfB should be(3f / maxFreq) // m1, m2, new B
    tfB2 should be(2f / maxFreq) // m1, new B2
    tfC should be(5f / maxFreq) // m1, m2, m3, new C, new B
    tfNothing should be(1f)
  }

  it should "calculate type frequencies at contravariant positions" in {
    val (tfs, maxFreq) = typeFrequenciesWithMaxAbsoluteFrequency("""
      package p

      trait A
      class B extends A
      class B2 extends A
      class C extends B

      object O {
       /** m1 */
       def m1(x: A) = ???
       def m2(x: B) = ???
       def m3(x: C) = ???
      }
      """)

    val tfAny = tfs((Contravariant, TypeRef.Any.name))
    val tfA = tfs((Contravariant, "p.A"))
    val tfB = tfs((Contravariant, "p.B"))
    val tfB2 = tfs((Contravariant, "p.B2"))
    val tfC = tfs((Contravariant, "p.C"))
    val tfNothing = tfs((Contravariant, TypeRef.Nothing.name))

    tfAny should be >= tfA
    tfA should (
      be >= (3f / maxFreq) and
      be > tfB)
    tfB should (
      be >= (2f / maxFreq) and
      be > tfC)
    tfC should (
      be >= (1f / maxFreq))
    tfNothing should be(0f)
  }

  it should "calculate type frequencies at invariant positions" in {
    val (tfs, maxFreq) = typeFrequenciesWithMaxAbsoluteFrequency("""
      package p

      trait A
      class B extends A
      class C extends B

      object O {
       def m1(x: Array[B]) = ???
      }
      """)

    val tfA = tfs((Invariant, "p.A"))
    val tfB = tfs((Invariant, "p.B"))
    val tfC = tfs((Invariant, "p.C"))

    tfA should be(0f)
    tfB should be(1f / maxFreq)
    tfC should be(0f)
  }

  it should "calculate type frequencies of generic types" in {
    val (tfs, maxFreq) = typeFrequenciesWithMaxAbsoluteFrequency("""
      package p

      class A[+T]
      class B extends A[Int]
      class C[+T] extends A[T]

      object O {
        def m1: A[Char] = ???
        def m2: A[Int] = ???
        def m3: B = ???
        def m4: B = ???
        def m5: C[Float] = ???
      }
      """)

    val tfA = tfs((Covariant, "p.A"))
    val tfB = tfs((Covariant, "p.B"))

    tfA should be(3f / maxFreq) // new A, m1, m2
    tfB should be(3f / maxFreq) // new B, m3, m4
  }

  def typeFrequenciesWithMaxAbsoluteFrequency(source: String) = {
    val entities = extractAll(source)
    val values = entities.collect { case t: ValueDef => t }
    val views = entities.flatMap(View.fromEntity(_)).sortBy(_.from.name)

    withValueIndex { valueIndex =>
      valueIndex.addEntities(values)
      withViewIndex { viewIndex =>
        viewIndex.addEntities(views)

        val values = valueIndex.allEntities().get

        (TypeFrequencies(viewIndex.findAlternativesWithDistance(_).get.map(_._1), values, Int.MaxValue),
          values.filter(!_.isOverride).length)
      }
    }
  }
}

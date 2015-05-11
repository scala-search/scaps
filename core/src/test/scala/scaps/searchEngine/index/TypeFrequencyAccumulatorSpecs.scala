package scaps.searchEngine.index

import org.scalatest.FlatSpec
import scaps.webapi._

class TypeFrequencyAccumulatorSpecs extends FlatSpec with IndexUtils {
  "the type frequency accumulator" should "calculate type frequencies at covariant positions" in {
    val tfs = typeFrequencies("""
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

    val tfAny = tfs((Covariant, TypeEntity.Any.name))
    val tfA = tfs((Covariant, "p.A"))
    val tfB = tfs((Covariant, "p.B"))
    val tfB2 = tfs((Covariant, "p.B2"))
    val tfC = tfs((Covariant, "p.C"))
    val tfNothing = tfs((Covariant, TypeEntity.Nothing.name))

    tfAny should be < tfA
    tfA should (
      be >= 1 and
      be < tfB)
    tfB should (
      be >= 2 and
      be < tfC)
    tfB2 should be(tfA + 1) // ctor of B2 also returns a B2
    tfC should (
      be >= 3 and
      be < tfNothing)
  }

  it should "calculate type frequencies at contravariant positions" in {
    val tfs = typeFrequencies("""
      package p

      trait A
      class B extends A
      class B2 extends A
      class C extends B

      object O {
       def m1(x: A) = ???
       def m2(x: B) = ???
       def m3(x: C) = ???
      }
      """)

    val tfAny = tfs((Contravariant, TypeEntity.Any.name))
    val tfA = tfs((Contravariant, "p.A"))
    val tfB = tfs((Contravariant, "p.B"))
    val tfB2 = tfs((Contravariant, "p.B2"))
    val tfC = tfs((Contravariant, "p.C"))

    tfAny should be > tfA
    tfA should (
      be >= 3 and
      be > tfB)
    tfB should (
      be >= 2 and
      be > tfC)
    //tfB2 should be(0)
    tfC should (
      be >= 1)
  }

  it should "calculate type frequencies at invariant positions" in {
    val tfs = typeFrequencies("""
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

    tfA should be(0)
    tfB should be(1)
    tfC should be(0)
  }

  def typeFrequencies(source: String) = {
    val entities = extractAll(source)
    val terms = entities.collect { case t: TermEntity => t }
    val classes = entities.collect { case c: ClassEntity => c }

    withTermIndex { termIndex =>
      termIndex.addEntities(terms)
      withClassIndex { classIndex =>
        classIndex.addEntities(classes)

        val acc = TypeFrequencyAccumulator(termIndex, classIndex)

        acc().withDefaultValue(0)
      }
    }
  }
}

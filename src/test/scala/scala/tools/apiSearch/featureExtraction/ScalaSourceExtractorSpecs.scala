package scala.tools.apiSearch.featureExtraction

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.tools.apiSearch.utils.CompilerAccess
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.apiSearch.model._
import scala.util.Random

class ScalaSourceExtractorSpecs extends FlatSpec with Matchers with CompilerAccess {
  val extractor = new ScalaSourceExtractor(compiler)

  "the scala source feature extractor" should "extract an entity in an object" in {
    shouldExtractTerms("""
      package p

      object O {
        val a = 1
      }
      """)("p.O.a")
  }

  it should "extract an entity in a class" in {
    shouldExtractTerms("""
      package p

      class C {
        val a = 1
      }
      """)("p.C#a")
  }

  it should "not extract inherited members" in {
    val entities = extractAllTerms("""
      package p

      class C
      """)

    val toString = entities.find(_.name.endsWith("toString"))

    toString should not be ('defined)
  }

  it should "extract doc comments" in {
    val entities = extractAllTerms("""
      package p

      object O {
        /**
         * A doc comment
         */
        def a = 1

        // A single line comment
        def b = 2

        /* A multi line comment */
        def c = 3

        /** A minimal doc comment */
        def d = 4
      }
      """)

    val comments = entities.map(_.comment).mkString("\n")

    comments should include("A doc comment")
    comments should not include ("A single line comment")
    comments should not include ("A multi line comment")
    comments should include("A minimal doc comment")
  }

  it should "extract simple types from values" in {
    extractTerms("""
      package p

      object O {
        val a = 1
      }
      """)(
      ("p.O.a", a => a.tpe should be(TypeEntity("scala.Int", Covariant))))
  }

  it should "extract method types" in {
    extractTerms("""
      package p

      object O {
        def m1: Int = 1
        def m2(): Int = 1
        def m3(i: Int): String = ""
        def m4(i: Int)(d: Double): String = ""
      }
      """)(
      ("p.O.m1", _.tpe.toString should be("+scala.Int")),
      ("p.O.m2", _.tpe.toString should be("+scala.Function0[+scala.Int]")),
      ("p.O.m3", _.tpe.toString should be("+scala.Function1[-scala.Int, +java.lang.String]")),
      ("p.O.m4", _.tpe.toString should be("+scala.Function1[-scala.Int, +scala.Function1[-scala.Double, +java.lang.String]]")))
  }

  it should "treat member access like function application" in {
    extractTerms("""
      package p

      trait T {
        def m1 = 1
        def m2(i: Int) = 1
      }
      """)(
      ("p.T#m1", _.tpe.toString should be("+scala.Function1[-p.T, +scala.Int]")),
      ("p.T#m2", _.tpe.toString should be("+scala.Function1[-p.T, +scala.Function1[-scala.Int, +scala.Int]]")))
  }

  it should "treat nested member access like function application" in {
    extractTerms("""
      package p

      trait Outer {
        trait Inner {
          def m = 1
        }
      }
      """)(
      ("p.Outer#Inner#m", _.tpe.toString should be("+scala.Function1[-p.Outer#Inner, +scala.Int]")))
  }

  it should "add correct variance annotations" in {
    extractTerms("""
      package p

      class Co[+T]
      class Contra[-T]
      class In[T]

      object O {
        val a: Contra[Int] = ???
        val b: Co[Int] = ???
        val c: In[Int] = ???
        val d: Co[Contra[Int]] = ???
        val e: Contra[Co[Int]] = ???
        val f: In[In[Int]] = ???
      }
      """)(
      ("p.O.a", _.tpe.toString should be("+p.Contra[-scala.Int]")),
      ("p.O.b", _.tpe.toString should be("+p.Co[+scala.Int]")),
      ("p.O.c", _.tpe.toString should be("+p.In[scala.Int]")),
      ("p.O.d", _.tpe.toString should be("+p.Co[+p.Contra[-scala.Int]]")),
      ("p.O.e", _.tpe.toString should be("+p.Contra[-p.Co[-scala.Int]]")),
      ("p.O.f", _.tpe.toString should be("+p.In[p.In[scala.Int]]")))
  }

  it should "handle method types like function types" in {
    val entities = extractAllTerms("""
      package p

      object O {
        def m(i: Int): Int = ???
        val f = m _
      }
      """)

    val m = entities.find(_.name == "p.O.m").get
    val f = entities.find(_.name == "p.O.f").get

    m.tpe should be(f.tpe)
  }

  it should "extract type parameters" in {
    extractTerms("""
      package q

      object O {
        def m[T](x: T): T = x
      }
      """)(
      ("q.O.m", m => {
        m.typeParameters should be(List(TypeParameterEntity("T")))
        m.tpe.toString should be("+scala.Function1[-T, +T]")
      }))
  }

  it should "extract type parameters with bounds" in {
    extractTerms("""
      package q

      trait Up

      object O {
        def m[T <: Up](x: T): T = x
      }
      """)(
      ("q.O.m", m => {
        m.typeParameters should be(List(TypeParameterEntity("T", lowerBound = "scala.Nothing", upperBound = "q.Up")))
        m.tpe.toString should be("+scala.Function1[-T, +T]")
      }))
  }

  it should "extract type parameters from classes" in {
    extractTerms("""
      package p

      class C[T] {
        def m1 = 1
        def m2(x: T): T
        def m3[A](y: A): T
      }
      """)(
      ("p.C#m1", _.typeParameters should be(List(TypeParameterEntity("T")))),
      ("p.C#m2", _.typeParameters should be(List(TypeParameterEntity("T")))),
      ("p.C#m3", _.typeParameters should be(List(TypeParameterEntity("T"), TypeParameterEntity("A")))))
  }

  it should "extract type parameters from nested classes" in {
    extractTerms("""
      package p

      class Outer[A] {
        class Inner[B] {
          def m = 1
        }
      }
      """)(
      ("p.Outer#Inner#m", _.typeParameters should be(List(TypeParameterEntity("A"), TypeParameterEntity("B")))))
  }

  def extractAllTerms(source: String): List[TermEntity] = {
    val randomFileName = s"${Random.nextInt()}.scala"
    extractor(new BatchSourceFile(randomFileName, source))._2.toBlocking.toList
  }

  def extractTerms(source: String)(entityHandlers: (String, TermEntity => Unit)*): Unit = {
    val entities = extractAllTerms(source)
    val names = entities.map(_.name)

    entityHandlers.foreach { handler =>
      names should contain(handler._1)
      withClue(s"number of entities with name ${handler._1}") {
        names.count(_ == handler._1) should be(1)
      }
      entities.find(_.name == handler._1).fold(throw new Exception)(handler._2)
    }
  }

  def shouldExtractTerms(source: String)(entityNames: String*): Unit =
    extractTerms(source)(entityNames.map(n => (n, (_: TermEntity) => ())): _*)
}

package scaps.featureExtraction

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.reflect.internal.util.BatchSourceFile
import scaps.webapi._
import scala.util.Random

class ScalaSourceExtractorSpecs extends FlatSpec with Matchers with ExtractionUtils {
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

  it should "extract inherited members" in {
    shouldExtractTerms("""
      package p

      class C
      """)("p.C#toString")
  }

  it should "decode operator names" in {
    shouldExtractTerms("""
      package p

      class C {
        def =:=(x: C): Boolean
      }
      """)("p.C#=:=")
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

    comments should (
      include("A doc comment") and
      not include ("A single line comment") and
      not include ("A multi line comment") and
      include("A minimal doc comment"))
  }

  it should "expand variables in doc comments (only locally)" in {
    extractTerms("""
      package p

      /** An object
       *
       *  @define info Hello, world!
       */
      object O {
        /** #info */
        def m = 1
      }
      """.replace('#', '$'))( // workaround to mute "possible missing interpolator" warning
      ("p.O.m", _.comment should include("Hello, world!")))
  }

  it should "extract simple types from values" in {
    extractTerms("""
      package p

      object O {
        val a = 1
      }
      """)(
      ("p.O.a", _.tpe should be(TypeEntity("scala.Int", Covariant, Nil))))
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
      ("p.O.m2", _.tpe.toString should be("+<methodInvocation0>[+scala.Int]")),
      ("p.O.m3", _.tpe.toString should be("+<methodInvocation1>[-scala.Int, +java.lang.String]")),
      ("p.O.m4", _.tpe.toString should be("+<methodInvocation1>[-scala.Int, +<methodInvocation1>[-scala.Double, +java.lang.String]]")))
  }

  it should "treat member access like function application" in {
    extractTerms("""
      package p

      trait T {
        def m1 = 1
        def m2(i: Int) = 1
      }
      """)(
      ("p.T#m1", _.tpe.toString should be("+<memberAccess>[-p.T, +scala.Int]")),
      ("p.T#m2", _.tpe.toString should be("+<memberAccess>[-p.T, +<methodInvocation1>[-scala.Int, +scala.Int]]")))
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
      ("p.Outer#Inner#m", _.tpe.toString should be("+<memberAccess>[-p.Outer#Inner, +scala.Int]")))
  }

  it should "include type args in owner type of member access" in {
    extractTerms("""
      package p

      trait T[A] {
        def m = 1
      }

      trait S[+A] {
        def m = 1
      }
      """)(
      ("p.T#m", _.tpe.toString should be("+<memberAccess>[-p.T[A], +scala.Int]")),
      ("p.S#m", _.tpe.toString should be("+<memberAccess>[-p.S[-A], +scala.Int]")))
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

  it should "extract type parameters" in {
    extractTerms("""
      package q

      object O {
        def m[T](x: T): T = x
      }
      """)(
      ("q.O.m", m => {
        m.typeParameters should be(List(TypeParameterEntity("T", Invariant)))
        m.tpe.toString should be("+<methodInvocation1>[-T, +T]")
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
        m.typeParameters should be(List(TypeParameterEntity("T", Invariant, lowerBound = "scala.Nothing", upperBound = "q.Up")))
        m.tpe.toString should be("+<methodInvocation1>[-T, +T]")
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
      ("p.C#m1", _.typeParameters should be(List(TypeParameterEntity("T", Invariant)))),
      ("p.C#m2", _.typeParameters should be(List(TypeParameterEntity("T", Invariant)))),
      ("p.C#m3", _.typeParameters should be(List(TypeParameterEntity("T", Invariant), TypeParameterEntity("A", Invariant)))))
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
      ("p.Outer#Inner#m", _.typeParameters should be(List(TypeParameterEntity("A", Invariant), TypeParameterEntity("B", Invariant)))))
  }

  it should "extract constructors as 'static' method named '<init>'" in {
    extractTerms("""
      package p

      class A(x: Int)

      class B[T](x: T)
      """)(
      ("p.A.<init>", _.tpe.toString should be("+<methodInvocation1>[-scala.Int, +p.A]")),
      ("p.B.<init>", _.tpe.toString should be("+<methodInvocation1>[-T, +p.B[T]]")))
  }

  it should "extract auxiliary constructors" in {
    val terms = extractAllTerms("""
      package p

      class A(x: Int) {
        def this() = this(1)
      }
      """)

    val ctorTypes = terms.filter(_.name == "p.A.<init>").map(_.tpe.toString)

    ctorTypes should have length (2)
    ctorTypes should contain("+<methodInvocation0>[+p.A]")
  }

  it should "not extract constructors of abstract classes" in {
    val terms = extractAllTerms("""
      package p

      trait T
      """)

    terms.find(_.name == "p.T#<init>") should not be ('defined)
  }

  it should "extract inherited members from super traits" in {
    extractTerms("""
      package p

      trait T {
        def m = 1
      }

      class C extends T
      object O extends T
      """)(
      ("p.T#m", _ => ()),
      ("p.C#m", _ => ()),
      ("p.O.m", _ => ()))
  }

  it should "extract types of inherited members with substituted type args" in {
    extractTerms("""
      package p

      trait T[A] {
        def m(a: A) = ()
      }

      object O extends T[Int]
      """)(
      ("p.O.m", _.tpe.toString should be("+<methodInvocation1>[-scala.Int, +scala.Unit]")))
  }

  it should "extract terms in package objects" in {
    extractTerms("""
      package object p{
        def m = 1
      }
      """)(
      ("p.m", _ => ()))
  }

  it should "extract objects as terms and classes" in {
    val src = """
      package p

      object O
      """
    extractClasses(src)(
      ("p.O$", _ => ()))
    extractTerms(src)(
      ("p.O", _ => ()))
  }

  it should "extract objects as values with a refinement type" in {
    extractTerms("""
      package p

      trait R
      trait S extends R
      trait T[A]

      object O
      object P extends S with T[Int]
      """)(
      ("p.O", _.tpe.toString should be("+<refinement1>[+java.lang.Object]")),
      ("p.P", _.tpe.toString should be("+<refinement3>[+java.lang.Object, +p.S, +p.T[scala.Int]]")))
  }

  it should "not extract private terms and classes" in {
    val entities = extractAll("""
      package p

      private trait T
      private object O

      object P {
        private trait T
        private object O
      }
      """)

    val privateNames = List("p.T", "p.O", "p.P.T", "p.P.O")
    val privateEntities = entities.filter(t => privateNames.contains(t.name))

    privateEntities should be('empty)
  }

  it should "extract class entities" in {
    extractClasses("""
      package p

      class C
      """)(
      ("p.C", _ => ()))
  }

  it should "decode class names" in {
    extractClasses("""
      package p

      class ::
      """)(
      ("p.::", _ => ()))
  }

  it should "extract traits into class entities" in {
    extractClasses("""
      package p

      trait T
      """)(
      ("p.T", _ => ()))
  }

  it should "extract base types of classes" in {
    extractClasses("""
      package p

      trait T

      class C extends T

      class D extends C with T

      class E extends C
      """)(
      ("p.C", _.baseTypes should (
        contain(TypeEntity("p.T", Covariant, Nil)) and
        contain(TypeEntity.Any()))),
      ("p.D", _.baseTypes should (
        contain(TypeEntity("p.C", Covariant, Nil)) and
        contain(TypeEntity("p.T", Covariant, Nil)))),
      ("p.E", _.baseTypes should (
        contain(TypeEntity("p.C", Covariant, Nil)) and
        contain(TypeEntity("p.T", Covariant, Nil)))))
  }

  it should "extract class entities with type parameters" in {
    extractClasses("""
      package p

      class C[T]
      """)(
      ("p.C", _.typeParameters should contain(TypeParameterEntity("T", Invariant))))
  }

  it should "use the concrete type arguments in base types" in {
    extractClasses("""
      package p

      trait T[A]

      class C extends T[Int]
      class D[B] extends T[B]
      """)(
      ("p.C", _.baseTypes.mkString(", ") should include("p.T[scala.Int]")),
      ("p.D", _.baseTypes.mkString(", ") should include("p.T[B]")))
  }

  it should "yield referenced types as class entities" in {
    extractClasses("""
      package scaps.featureExtraction

      object O{
        def x = "hi"
      }
      """)(
      ("java.lang.String", _ => ()))
  }
}

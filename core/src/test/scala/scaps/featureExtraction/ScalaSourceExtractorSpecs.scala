package scaps.featureExtraction

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.reflect.internal.util.BatchSourceFile
import scaps.api._
import scala.util.Random

class ScalaSourceExtractorSpecs extends FlatSpec with Matchers with ExtractionUtils {
  "the scala source feature extractor" should "extract a value in an object" in {
    shouldExtractValues("""
      package p

      object O {
        val a = 1
      }
      """)("p.O.a")
  }

  it should "extract a value in a class" in {
    shouldExtractValues("""
      package p

      class C {
        val a = 1
      }
      """)("p.C.a")
  }

  it should "extract inherited members" in {
    shouldExtractValues("""
      package p

      class C
      """)("p.C.toString")
  }

  it should "decode operator names" in {
    shouldExtractValues("""
      package p

      class C {
        def =:=(x: C): Boolean
      }
      """)("p.C.=:=")
  }

  it should "extract doc comments" in {
    val entities = extractAllValues("""
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
    extractValues("""
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
    extractValues("""
      package p

      object O {
        val a = 1
      }
      """)(
      ("p.O.a", m => {
        m.tpe should be(TypeRef("scala.Int", Covariant, Nil))
        m.tpe.args.foreach(_.isTypeParam should be(false))
      }))
  }

  it should "extract method types" in {
    extractValues("""
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
    extractValues("""
      package p

      trait T {
        def m1 = 1
        def m2(i: Int) = 1
      }
      """)(
      ("p.T.m1", _.tpe.toString should be("+<memberAccess>[-p.T, +scala.Int]")),
      ("p.T.m2", _.tpe.toString should be("+<memberAccess>[-p.T, +<methodInvocation1>[-scala.Int, +scala.Int]]")))
  }

  it should "treat nested member access like function application" in {
    extractValues("""
      package p

      trait Outer {
        trait Inner {
          def m = 1
        }
      }
      """)(
      ("p.Outer.Inner.m", _.tpe.toString should be("+<memberAccess>[-p.Outer.Inner, +scala.Int]")))
  }

  it should "include type args in owner type of member access" in {
    extractValues("""
      package p

      trait T[A] {
        def m = 1
      }

      trait S[+A] {
        def m = 1
      }
      """)(
      ("p.T.m", _.tpe.toString should be("+<memberAccess>[-p.T[A], +scala.Int]")),
      ("p.S.m", _.tpe.toString should be("+<memberAccess>[-p.S[-A], +scala.Int]")))
  }

  it should "add correct variance annotations" in {
    extractValues("""
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
    extractValues("""
      package q

      object O {
        def m[T](x: T): T = x
      }
      """)(
      ("q.O.m", m => {
        m.typeParameters should be(List(TypeParameter("T", Invariant)))
        m.tpe.toString should be("+<methodInvocation1>[-T, +T]")
        m.tpe.args.foreach(_.isTypeParam should be(true))
      }))
  }

  it should "extract type parameters with bounds" in {
    extractValues("""
      package q

      trait Up

      object O {
        def m[T <: Up](x: T): T = x
      }
      """)(
      ("q.O.m", m => {
        m.typeParameters should be(List(TypeParameter("T", Invariant, lowerBound = "scala.Nothing", upperBound = "q.Up")))
        m.tpe.toString should be("+<methodInvocation1>[-T, +T]")
        m.tpe.args.foreach(_.isTypeParam should be(true))
      }))
  }

  it should "extract type parameters from owners" in {
    extractValues("""
      package p

      class C[T] {
        def m1 = 1
        def m2(x: T): T
        def m3[A](y: A): T
      }
      """)(
      ("p.C.m1", _.typeParameters should be(List(TypeParameter("T", Invariant)))),
      ("p.C.m2", m => {
        m.typeParameters should be(List(TypeParameter("T", Invariant)))
        m.tpe.args(1).args.foreach(_.isTypeParam should be(true))
      }),
      ("p.C.m3", m => {
        m.typeParameters should be(List(TypeParameter("T", Invariant), TypeParameter("A", Invariant)))
        m.tpe.args(1).args.foreach(_.isTypeParam should be(true))
      }))
  }

  it should "extract type parameters from nested typeDefs" in {
    extractValues("""
      package p

      class Outer[A] {
        class Inner[B] {
          def m = 1
        }
      }
      """)(
      ("p.Outer.Inner.m", m => {
        m.typeParameters should be(List(TypeParameter("A", Invariant), TypeParameter("B", Invariant)))
        m.tpe.args(1).args.foreach(_.isTypeParam should be(true))
      }))
  }

  it should "extract constructors as 'static' method named '<init>'" in {
    extractValues("""
      package p

      class A(x: Int)

      class B[T](x: T)
      """)(
      ("p.A.<init>", _.tpe.toString should be("+<methodInvocation1>[-scala.Int, +p.A]")),
      ("p.B.<init>", _.tpe.toString should be("+<methodInvocation1>[-T, +p.B[T]]")))
  }

  it should "extract auxiliary constructors" in {
    val values = extractAllValues("""
      package p

      class A(x: Int) {
        def this() = this(1)
      }
      """)

    val ctorTypes = values.filter(_.name == "p.A.<init>").map(_.tpe.toString)

    ctorTypes should have length (2)
    ctorTypes should contain("+<methodInvocation0>[+p.A]")
  }

  it should "not extract constructors of abstract classes and traits" in {
    val values = extractAllValues("""
      package p

      trait T
      """)

    values.find(_.name == "p.T.<init>") should not be ('defined)
  }

  it should "extract inherited members from super traits" in {
    extractValues("""
      package p

      trait T {
        def m = 1
      }

      class C extends T
      object O extends T
      """)(
      ("p.T.m", _ => ()),
      ("p.C.m", _ => ()),
      ("p.O.m", _ => ()))
  }

  it should "extract types of inherited members with substituted type args" in {
    extractValues("""
      package p

      trait T[A] {
        def m(a: A) = ()
      }

      object O extends T[Int]
      """)(
      ("p.O.m", _.tpe.toString should be("+<methodInvocation1>[-scala.Int, +scala.Unit]")))
  }

  it should "extract values in package objects" in {
    extractValues("""
      package object p{
        def m = 1
      }
      """)(
      ("p.m", _ => ()))
  }

  it should "extract objects as values and typeDefs" in {
    val src = """
      package p

      object O
      """
    extractTypeDefs(src)(
      ("p.O$", _ => ()))
    extractValues(src)(
      ("p.O", _ => ()))
  }

  it should "extract objects as values with a refinement type" in {
    extractValues("""
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

  it should "extract refinement types of values and methods" in {
    extractValues("""
      package p

      trait T {}
      trait U {}

      object O {
        val t = new T {}
        def m = new T {}
        val u = new T with U {}
      }
      """)(
      ("p.O.t", _.tpe.toString should be("+<refinement2>[+java.lang.Object, +p.T]")),
      ("p.O.m", _.tpe.toString should be("+<refinement2>[+java.lang.Object, +p.T]")),
      ("p.O.u", _.tpe.toString should be("+<refinement3>[+java.lang.Object, +p.T, +p.U]")))
  }

  it should "extract by name parameters" in {
    extractValues("""
      package p

      object O {
        def m(v: => Int) = v
      }
      """)(
      ("p.O.m", _.tpe.args should contain(TypeRef.ByName(TypeRef.Int(Contravariant), Contravariant))))
  }

  it should "extract by repeated parameters" in {
    extractValues("""
      package p

      object O {
        def m(v: Int*) = v
      }
      """)(
      ("p.O.m", _.tpe.args should contain(TypeRef.Repeated(TypeRef.Int(Contravariant), Contravariant))))
  }

  it should "extract implicit parameters" in {
    extractValues("""
      package p

      object O {
        def m(implicit i: Int) = ???

        def n[T: Option](o: T) = ???
      }
      """)(
      ("p.O.m", _.tpe.args should contain(TypeRef.Implicit(TypeRef.Int(Contravariant), Contravariant))),
      ("p.O.n", _.tpe.args(1).args should contain(
        TypeRef.Implicit(
          TypeRef.Option(
            TypeRef("T", Contravariant, Nil, isTypeParam = true), Contravariant), Contravariant))))
  }

  it should "not extract private values and typeDefs" in {
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

  it should "extract type definitions" in {
    extractTypeDefs("""
      package p

      class C
      """)(
      ("p.C", _ => ()))
  }

  it should "decode type names" in {
    extractTypeDefs("""
      package p

      class ::
      """)(
      ("p.::", _ => ()))
  }

  it should "extract traits into type definitions" in {
    extractTypeDefs("""
      package p

      trait T
      """)(
      ("p.T", _ => ()))
  }

  it should "extract base types" in {
    extractTypeDefs("""
      package p

      trait T

      class C extends T

      class D extends C with T

      class E extends C
      """)(
      ("p.C", _.baseTypes should (
        contain(TypeRef("p.T", Covariant, Nil)) and
        contain(TypeRef.Any()))),
      ("p.D", _.baseTypes should (
        contain(TypeRef("p.C", Covariant, Nil)) and
        contain(TypeRef("p.T", Covariant, Nil)))),
      ("p.E", _.baseTypes should (
        contain(TypeRef("p.C", Covariant, Nil)) and
        contain(TypeRef("p.T", Covariant, Nil)))))
  }

  it should "extract type definitions with type parameters" in {
    extractTypeDefs("""
      package p

      class C[T]
      """)(
      ("p.C", _.typeParameters should contain(TypeParameter("T", Invariant))))
  }

  it should "use the concrete type arguments in base types" in {
    extractTypeDefs("""
      package p

      trait T[A]

      class C extends T[Int]
      class D[B] extends T[B]
      """)(
      ("p.C", _.baseTypes.mkString(", ") should include("p.T[scala.Int]")),
      ("p.D", cls => {
        cls.baseTypes.mkString(", ") should include("p.T[B]")
        val tBase = cls.baseTypes.find(_.name == "p.T").get
        tBase.args.foreach(_.isTypeParam should be(true))
      }))
  }

  it should "yield referenced types as type definitions" in {
    extractTypeDefs("""
      package p

      object O{
        def x = "hi"
      }
      """)(
      ("java.lang.String", _ => ()))
  }

  it should "set the 'overrides' flag on values that override an inherited member" in {
    extractValues("""
      package p

      trait T {
        def m = 1
      }

      class A extends T {
        override def m = 2
        def n = 1
      }

      class B extends T {
        def n = 1
      }

      object O extends T {
        override def m = 2
        def n = 1
      }

      object P extends T {
        def n = 1
      }
      """)(
      ("p.A.m", _.isOverride should be(true)),
      ("p.A.n", _.isOverride should be(false)),
      ("p.B.m", _.isOverride should be(true)),
      ("p.B.n", _.isOverride should be(false)),
      ("p.O.m", _.isOverride should be(true)),
      ("p.O.n", _.isOverride should be(false)),
      ("p.P.m", _.isOverride should be(true)),
      ("p.P.n", _.isOverride should be(false)))
  }

  it should "set the 'implicit' flag on implicit defs" in {
    extractValues("""
      package p

      object O{
        implicit def m(l: Long): Int = ???
      }
      """)(
      ("p.O.m", _.isImplicit should be(true)))
  }

  it should "set the 'implicit' flag on primary constructors of implicit classes" in {
    extractValues("""
      package p

      object O {
        implicit class C(i: Int)
      }
      """)(
      ("p.O.C.<init>", _.isImplicit should be(true)))
  }

  it should "set the 'static' flag on values that have no classes in their owner chain" in {
    extractValues("""
      package p

      object O {
        def m = 1

        object P {
          def n = 1
        }

        class C {
          def o = 1

          object Q {
            def p = 1
          }
        }
      }
      """)(
      ("p.O", _.isStatic should be(true)),
      ("p.O.m", _.isStatic should be(true)),
      ("p.O.P", _.isStatic should be(true)),
      ("p.O.P.n", _.isStatic should be(true)),
      ("p.O.P.toString", _.isStatic should be(true)),
      ("p.O.C.<init>", _.isStatic should be(true)),
      ("p.O.C.o", _.isStatic should be(false)),
      ("p.O.C.Q", _.isStatic should be(false)),
      ("p.O.C.Q.p", _.isStatic should be(false)))
  }

  it should "extract scala doc identifiers" in {
    extractValues("""
      package p

      object O {
        def m = 1

        object P {
          def n(a: String) = 1.0
        }
      }

      class C {
        def ++(a: String) = a
      }
      """)(
      ("p.O.m", _.docLink should be(Some("p.O$@m:Int"))),
      ("p.O.P.n", _.docLink should be(Some("p.O$$P$@n(a:String):Double"))),
      ("p.O", _.docLink should be(Some("p.O$"))),
      ("p.C.++", _.docLink should be(Some("p.C@++(a:String):String"))),
      ("p.C.toString", _.docLink should be(Some("p.C@toString():String"))))
  }
}

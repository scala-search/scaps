/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.scala.featureExtraction

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.reflect.internal.util.BatchSourceFile
import scaps.api._

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

  it should "not treat inherited members of objects as member access" in {
    extractValues("""
      package p

      trait T {
        def m(i: Int) = i
      }

      object O extends T
      """)(
      ("p.O.m", _.tpe.toString should be("+<methodInvocation1>[-scala.Int, +scala.Int]")))
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
        m.typeParameters should be(List(
          TypeParameter("T", Invariant, lowerBound = TypeRef.Nothing(Covariant), upperBound = TypeRef("q.Up", Contravariant, Nil))))
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

  it should "extract type definitions with type parameters" in {
    extractTypeDefs("""
      package p

      class C[T]
      """)(
      ("p.C", _.typeParameters should contain(TypeParameter("T", Invariant))))
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

  it should "extract scala doc links" in {
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

  it should "extract views from subtype relations" in {
    val views = extractAllViews("""
      package p

      class A
      class B extends A
      """)

    views.map(_.name) should (
      contain("p.A:-p.A:-java.lang.Object") and
      contain("p.A:+java.lang.Object:+p.A") and
      contain("p.A:-p.A:-scala.Any") and
      contain("p.A:+scala.Any:+p.A") and
      contain("p.B:-p.B:-p.A") and
      contain("p.B:+p.A:+p.B") and
      contain("p.B:-p.B:-java.lang.Object") and
      contain("p.B:+java.lang.Object:+p.B") and
      contain("p.B:-p.B:-scala.Any") and
      contain("p.B:+scala.Any:+p.B"))
  }

  it should "extract views from implicit conversion methods" in {
    val views = extractAllViews("""
      package p

      object O {
        implicit def int2long(i: Int): Long = ???

        // this really declares a implicit conversion from `Int` to `Long => String`:
        implicit def int2fn(i: Int)(l: Long): String = ???
      }
      """)

    views.map(_.name) should (
      contain("p.O.int2long:-scala.Int:-scala.Long") and
      contain("p.O.int2long:+scala.Long:+scala.Int") and
      contain("p.O.int2fn:-scala.Int:-scala.Function1[+scala.Long, -java.lang.String]") and
      contain("p.O.int2fn:+scala.Function1[-scala.Long, +java.lang.String]:+scala.Int"))
  }

  it should "extract views from implicit conversion functions" in {
    val views = extractAllViews("""
      package p

      object O {
        implicit val int2long: Int => Long = ???
        implicit val int2fn: Int => Long => String = ???
      }
      """)

    views.map(_.name) should (
      contain("p.O.int2long:-scala.Int:-scala.Long") and
      contain("p.O.int2long:+scala.Long:+scala.Int") and
      contain("p.O.int2fn:-scala.Int:-scala.Function1[+scala.Long, -java.lang.String]") and
      contain("p.O.int2fn:+scala.Function1[-scala.Long, +java.lang.String]:+scala.Int"))
  }

  it should "extract views from implicit conversion classes" in {
    val views = extractAllViews("""
      package p

      object O {
        implicit class IntExt(i: Int)
      }
      """)

    views.map(_.name) should (
      contain("p.O.IntExt.<init>:-scala.Int:-p.O.IntExt") and
      contain("p.O.IntExt.<init>:+p.O.IntExt:+scala.Int"))
  }

  it should "ignore conversions requiring implicit parameters" in {
    val views = extractAllViews("""
      package p

      object O {
        implicit def int2long(i: Int)(implicit ev: String): Long = ???
        implicit def int2float[T: Ordering](i: Int): Float = ???
      }
      """)

    views.map(_.name) should (
      not contain ("p.O.int2long:-scala.Int:-scala.Long") and
      not contain ("p.O.int2long:+scala.Long:+scala.Int") and
      not contain ("p.O.int2float:-scala.Int:-scala.Float") and
      not contain ("p.O.int2float:+scala.Float:+scala.Int"))
  }

  it should "create views for all subtypes of scala.Seq to <repeated>" in {
    val views = extractAllViews("""
      package p

      trait S[+T] extends Seq[T]
      """)

    views.map(_.name) should (
      contain(":-p.S[-_]:-scala.<repeated>[-_]") and
      contain(":+scala.<repeated>[+_]:+p.S[+_]"))
  }

  it should "create views from parametrized types to parametrized top and bottom types" in {
    val views = extractAllViews("""
      package p

      trait A[+T]
      trait B[-T, +U]
      trait C[T, U, V]
      """)

    views.map(_.name) should (
      contain(":-p.A[-_]:-<top1>[-_]") and
      contain(":-p.B[+_, -_]:-<top2>[+_, -_]") and
      contain(":-p.C[_, _, _]:-<top3>[_, _, _]") and
      contain(":+p.A[+_]:+<bottom1>[+_]") and
      contain(":+p.B[-_, +_]:+<bottom2>[-_, +_]") and
      contain(":+p.C[_, _, _]:+<bottom3>[_, _, _]"))
  }

  it should "transform views to a type with argument Nothing to views to a parametrized type" in {
    val views = extractAllViews("""
      package p

      trait Box[+T]
      object Empty extends Box[Nothing]
      """)

    views.map(_.name) should (
      contain("p.Empty$:+p.Box[+_]:+p.Empty$") and
      contain("p.Empty$:-p.Empty$:-p.Box[-_]"))
  }
}

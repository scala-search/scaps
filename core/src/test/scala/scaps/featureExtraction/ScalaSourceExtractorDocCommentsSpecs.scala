package scaps.featureExtraction

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.api.ValueDef

class ScalaSourceExtractorDocCommentsSpecs extends FlatSpec with Matchers with ExtractionUtils {

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

  it should "inherit doc comments" in {
    extractValues("""
      package p
      
      class A {
        /** Base comment */
        def m = 1
      }
      
      class B extends A
      """)(
      ("p.B.m", _.comment should include("Base comment")))
  }

  it should "inherit doc comments from base classes in other files" in {
    val entities = extractAll(
      """
        package p
        
        class A {
          /** Base comment */
          def m = 1
        }
        """,
      """
        package p
        
        class B extends A
        """)

    val bm = entities.collectFirst { case v: ValueDef if v.name == "p.B.m" => v }.get

    bm.comment should include("Base comment")
  }

  it should "expand variables defined in base class" in {
    val entities = extractAll(
      """
        package p
        
        /** 
         * @define info Hello, world!
         */
        class A
        """,
      """
        package p
        
        class B extends A {
          /** #info */
          def m = 1
        }
        """.replace('#', '$'))

    val bm = entities.collectFirst { case v: ValueDef if v.name == "p.B.m" => v }.get

    bm.comment should include("Hello, world!")
  }

}
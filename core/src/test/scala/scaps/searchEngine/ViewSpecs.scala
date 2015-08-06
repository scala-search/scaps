package scaps.searchEngine

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.featureExtraction.ExtractionUtils
import scaps.webapi.TypeEntity

class ViewSpecs extends FlatSpec with Matchers with ExtractionUtils {
  "the views creator" should "create views from implicit defs" in {
    val is = views("""
      package p

      object O {
        implicit def int2long(i: Int): Long = ???

        // this really declares a implicit conversion from `Int` to `Long => String`:
        implicit def int2fn(i: Int)(l: Long): String = ???
      }
      """)
      .collect { case i: ImplicitConversion => i }

    val i2l = is.find(_.evidence.contains("int2long")).get
    i2l.from should be(TypeEntity.Int())
    i2l.to should be(TypeEntity.Long())

    val i2fn = is.find(_.evidence.contains("int2fn")).get
    i2fn.from should be(TypeEntity.Int())
    i2fn.to should be(TypeEntity.Function(TypeEntity.Long() :: Nil, TypeEntity.String()))
  }

  it should "create views from implicit vals of function type" in {
    val is = views("""
      package p

      object O {
        implicit val int2long: Int => Long = ???
        implicit val int2fn: Int => Long => String = ???
      }
      """)
      .collect { case i: ImplicitConversion => i }

    val i2l = is.find(_.evidence.contains("int2long")).get
    i2l.from should be(TypeEntity.Int())
    i2l.to should be(TypeEntity.Long())

    val i2fn = is.find(_.evidence.contains("int2fn")).get
    i2fn.from should be(TypeEntity.Int())
    i2fn.to should be(TypeEntity.Function(TypeEntity.Long() :: Nil, TypeEntity.String()))
  }

  it should "create views from implicit classes" in {
    val i2l = views("""
      package p

      object O {
        implicit class IntExt(i: Int)
      }
      """)
      .collectFirst { case i: ImplicitConversion => i }
      .get

    i2l.from should be(TypeEntity.Int())
    i2l.to.name should be("p.O.IntExt")
  }

  it should "ignore implicit params and context bounds in conversions" in {
    val vs = views("""
      package p

      object O {
        implicit def int2long(i: Int)(implicit ev: String): Long = ???
        implicit def int2float[T: Ordering](i: Int): Float = ???
      }
      """)
      .collect { case i: ImplicitConversion => i }

    val i2l = vs.find(_.evidence.contains("int2long")).get

    i2l.from should be(TypeEntity.Int())
    i2l.to should be(TypeEntity.Long())

    val i2f = vs.find(_.evidence.contains("int2float")).get

    i2f.from should be(TypeEntity.Int())
    i2f.to should be(TypeEntity.Float())
  }

  it should "create views for all subtypes of scala.Seq to <repeated>" in {
    val repeatedViews = views("""
      package p

      object O {
        val l = List(1)
        val s = Seq(1)
      }
      """)
      .collect { case i @ ImplicitConversion(_, TypeEntity.Repeated(_, _), _) => i }

    val l2r = repeatedViews.find(_.from.name == TypeEntity.SList.name).get

    l2r should matchPattern {
      case ImplicitConversion(TypeEntity.SList(arg1, _), TypeEntity.Repeated(arg2, _), _) if arg1 == arg2 =>
    }

    val s2r = repeatedViews.find(_.from.name == TypeEntity.Seq.name).get

    s2r should matchPattern {
      case ImplicitConversion(TypeEntity.Seq(arg1, _), TypeEntity.Repeated(arg2, _), _) if arg1 == arg2 =>
    }
  }

  def views(source: String) =
    extractAll(source).flatMap(View.fromEntity)
}

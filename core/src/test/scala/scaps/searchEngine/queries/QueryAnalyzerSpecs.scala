package scaps.searchEngine.queries

import scala.collection.immutable.Map
import org.scalatest.FlatSpec
import scaps.featureExtraction.ExtractionUtils
import scaps.searchEngine.NameAmbiguous
import scaps.searchEngine.NameNotFound
import scaps.searchEngine.SearchEngine
import scaps.searchEngine.UnexpectedNumberOfTypeArgs
import scaps.settings.Settings
import scaps.webapi.TypeEntity
import scaps.searchEngine.View
import scaps.webapi.Covariant
import scaps.webapi.Contravariant
import scaps.webapi.Invariant
import org.apache.lucene.store.RAMDirectory
import scaps.searchEngine.index.ViewIndex

class QueryAnalyzerSpecs extends FlatSpec with ExtractionUtils {

  // the namespace used for all analyzer tests
  // in order to make sure all required classes from scala stdlib are loaded
  // they are referenced in `loadTypes.TypeToBeLoaded`
  val env = """
    package p {
      class Aa
      class Bb extends Aa
      class Cc extends Bb
      class Dd extends Bb

      class Ambiguous

      class List[+T]
    }

    package q {
      class Ambiguous
    }

    package scala.pkg {
      class Int
    }

    package scaps.searchEngine.queries {
      trait TypesToBeLoaded {
        val char: Char
        val float: Float
        val f1: Any => Any
        val f2: (Any, Any) => Any
        val f3: (Any, Any, Any) => Any
        val tuple1: Tuple1[Any]
        val tuple2: (Any, Any)
        val tuple3: (Any, Any, Any)
        val list: List[Any]
      }
    }
    """

  "the query analyzer" should "resolve type names" in {
    val res = expectSuccess("Aa")

    res.allAlternatives.mkString(" ") should include("p.Aa")
  }

  it should "fail on unknown names" in {
    val res = expectFailure("Unknown")

    res should be(a[NameNotFound])
  }

  it should "return suggestions on ambiguous names" in {
    val res = expectFailure("Ambiguous")

    res should be(a[NameAmbiguous])
  }

  it should "fail on using names with incorrect number of arguments" in {
    val res = expectFailure("List[A, B]")

    res should be(a[UnexpectedNumberOfTypeArgs])
  }

  it should "succeed when using no type arguments" in {
    expectSuccess("List")
    ()
  }

  it should "prefer names from the Scala standard library over other namespaces" in {
    val res = expectSuccess("List")

    res.allAlternatives.mkString(" ") should (
      include("scala.collection.immutable.List") and
      not include ("p.List"))
  }

  it should "prefer names from the `scala` root namespace over names from subpackages of `scala`" in {
    val res = expectSuccess("Int")

    res.allAlternatives.mkString(" ") should (
      include("scala.Int") and
      not include ("scala.pkg.Int"))
  }

  it should "correctly trace variance in nested type constructor applications" in {
    val res = expectSuccess("(Aa => Bb) => (Cc => Dd)")

    res.allAlternatives.mkString(" ") should (
      include("+p.Aa_0") and
      include("-p.Bb_0") and
      include("-p.Cc_0") and
      include("+p.Dd_0"))
  }

  it should "add increasing occurrence numbers to repeated elements" in {
    val res = expectSuccess("(Aa, Aa, Aa)")

    res.allAlternatives.mkString(" ") should (
      include("+p.Aa_0") and
      include("+p.Aa_1") and
      include("+p.Aa_2"))
  }

  it should "include sub classes of types at covariant positions" in {
    val res = expectSuccess("_ => Aa")

    res.allAlternatives.mkString(" ") should (
      include("p.Bb") and
      include("p.Cc") and
      include("p.Dd"))
  }

  it should "not use the bottom type as a sub class of every type" in {
    val res = expectSuccess("_ => Aa")

    res.allAlternatives.mkString(" ") should (
      not include ("+scala.Nothing"))
  }

  it should "include base classes of types at contravariant positions" in {
    val res = expectSuccess("Cc => _")

    res.allAlternatives.mkString(" ") should (
      include("p.Bb") and
      include("p.Aa") and
      include("scala.Any"))
  }

  it should "yield a lower boost for types in deeper nested positions" in {
    val res = expectSuccess("(Float, (Int, _))")

    val Float = res.allAlternatives.find(_.typeName == TypeEntity.Float.name).get
    val Int = res.allAlternatives.find(_.typeName == TypeEntity.Int.name).get

    Float.boost should be > (Int.boost)
  }

  it should "yield a boost of 1 for a single type" in {
    val res = expectSuccess("Aa")

    val Aa = res.allAlternatives.find(_.typeName == "p.Aa").get

    Aa.boost should be(1d +- 0.01f)
  }

  it should "omit the outermost function application" in {
    val res = expectSuccess("Aa => Bb")

    res.allAlternatives.mkString(" ") should not include ("Function1")
  }

  it should "normalize curried querries" in {
    val res1 = expectSuccess("Aa => Bb => Cc")
    val res2 = expectSuccess("(Aa, Bb) => Cc")

    res1 should equal(res2)

    val res3 = expectSuccess("Aa => (Bb, Cc) => Dd")
    val res4 = expectSuccess("(Aa, Bb, Cc) => Dd")

    res3 should equal(res4)
  }

  it should "preserve function arguments in higher kinded queries" in {
    val res = expectSuccess("(Aa => Bb) => Cc")

    res.allAlternatives.mkString(" ") should include("-scala.Function1_0")
  }

  def expectSuccess(s: String) = {
    val res = analyzer(QueryParser(s).getOrElse(???))
    res should be('right)
    res.getOrElse(???)
  }

  def expectFailure(s: String) = {
    val res = analyzer(QueryParser(s).getOrElse(???))
    res should be('left)
    res.swap.getOrElse(???)
  }

  val settings = {
    val s = Settings.fromApplicationConf
    val query = s.query.copy(typeFrequencyWeight = 0)
    s.copy(query = query)
  }

  val analyzer = {
    val classEntities = extractAllClasses(env)
    val views = classEntities.flatMap(View.fromClass(_))

    def toMultiMap[K, V](ps: Seq[(K, V)]): Map[K, List[V]] = ps
      .distinct
      .foldLeft(Map[K, List[V]]()) { (acc, keyAndValue) =>
        val values = acc.getOrElse(keyAndValue._1, Nil)
        acc + (keyAndValue._1 -> (keyAndValue._2 :: values))
      }
      .withDefaultValue(Nil)

    val findClassesBySuffix = toMultiMap(for {
      cls <- classEntities
      suffix <- cls.name.split("\\.").toList.tails.filterNot(_ == Nil).map(_.mkString("."))
    } yield (suffix, cls)) andThen (SearchEngine.favorScalaStdLib _)

    val viewsIndex = new ViewIndex(new RAMDirectory)
    viewsIndex.addEntities(views)

    new QueryAnalyzer(settings, findClassesBySuffix, viewsIndex.findViews(_).get)
  }
}

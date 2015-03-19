package scala.tools.apiSearch.searching

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.tools.apiSearch.model._
import scala.util.Try
import scala.collection.immutable.Map
import scala.util.Failure

class QueryAnalyzerSpecs extends FlatSpec with Matchers {
  "the query analyzer" should "resolve type names" in {
    val A = ClassEntity("p.A", Nil, Nil)
    val env = Map(
      "A" -> Try(List(A)))
    val analyzer = new QueryAnalyzer(env, _ => Try(Nil))

    analyzer(RawQuery("A")).get should be(Right(
      APIQuery(
        Part(Covariant, "p.A" :: Nil) :: Nil)))
  }

  it should "treat unknown names as type parameters" in {
    val analyzer = new QueryAnalyzer(_ => Try(Nil), _ => Try(Nil))

    analyzer(RawQuery("A")).get should be(Right(
      APIQuery(Nil)))
  }

  it should "return suggestions on ambiguous names" in {
    val pA = ClassEntity("p.A", Nil, Nil)
    val qA = ClassEntity("q.A", Nil, Nil)
    val env = Map(
      "A" -> Try(List(pA, qA)))
    val analyzer = new QueryAnalyzer(env, _ => Try(Nil))

    analyzer(RawQuery("A")).get should be('left)
  }

  it should "fail when class finder fails" in {
    val analyzer = new QueryAnalyzer(_ => Failure(new Exception), _ => Try(Nil))

    analyzer(RawQuery("A")) should be('failure)
  }
}

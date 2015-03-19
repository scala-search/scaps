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
      ("A", 0) -> Try(List(A)))
    val analyzer = new QueryAnalyzer(Function.untupled(env))

    analyzer(RawQuery("A")).get should be(Right(
      Query(
        Part(Covariant, "p.A" :: Nil) :: Nil)))
  }

  it should "return a left when class cannot be found" in {
    val analyzer = new QueryAnalyzer((_, _) => Try(Nil))

    analyzer(RawQuery("A")).get should be('left)
  }

  it should "fail when class finder fails" in {
    val analyzer = new QueryAnalyzer((_, _) => Failure(new Exception))

    analyzer(RawQuery("A")) should be('failure)
  }
}

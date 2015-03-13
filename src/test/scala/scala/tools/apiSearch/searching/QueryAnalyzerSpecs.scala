package scala.tools.apiSearch.searching

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.tools.apiSearch.model._
import scala.util.Try
import scala.collection.immutable.Map

class QueryAnalyzerSpecs extends FlatSpec with Matchers {
  "the query analyzer" should "resolve types" in {
    val A = ClassEntity("p.A", Nil, Nil)
    val env = Map(
      ("A", 0) -> Try(List(A)))
    val analyzer = new QueryAnalyzer(Function.untupled(env))

    analyzer.resolveTypes(RawQuery("A")).get should be(Right(RawQuery(A.name, Nil, Some(A))))
  }

  it should "fail when class is not indexed" in {
    val analyzer = new QueryAnalyzer((_, _) => Try(Nil))

    analyzer.resolveTypes(RawQuery("A")).get should be('left)
  }
}

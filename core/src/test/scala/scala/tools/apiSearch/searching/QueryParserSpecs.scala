package scala.tools.apiSearch.searching

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.language.postfixOps

class QueryParserSpecs extends FlatSpec with Matchers {
  "the query parser" should "parse simple types" in {
    QueryParser("Int") shouldBe RawQuery("Int")
  }

  it should "parse types with arguments" in {
    QueryParser("List[Int]") shouldBe RawQuery("List", List(RawQuery("Int")))
    QueryParser("Map[Int, String]") shouldBe RawQuery("Map", List(RawQuery("Int"), RawQuery("String")))
    QueryParser("List[Option[String]]") shouldBe RawQuery("List", List(RawQuery("Option", List(RawQuery("String")))))
  }

  it should "fail on empty argument lists" in {
    QueryParser("List[]") shouldFail
  }

  it should "parse namespaces" in {
    QueryParser("p.q.C") shouldBe RawQuery("p.q.C")
    QueryParser("p.q.C[argument.Name]") shouldBe RawQuery("p.q.C", List(RawQuery("argument.Name")))
  }

  it should "parse type projections" in {
    QueryParser("Outer#Inner") shouldBe RawQuery("Outer#Inner")
    QueryParser("Outer#Inner[A#B]") shouldBe RawQuery("Outer#Inner", List(RawQuery("A#B")))
  }

  implicit class ParserResultExtensions(res: Either[String, RawQuery]) {
    def shouldBe(expected: RawQuery) = {
      res should be('right)
      res.right.get should be(expected)
    }

    def shouldFail = {
      res should be('left)
    }
  }
}

package scala.tools.apiSearch.searching

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.language.postfixOps

class QueryParserSpecs extends FlatSpec with Matchers {
  "the query parser" should "parse simple types" in {
    "Int" shouldBeParsedAs RawQuery("Int")
  }

  it should "parse types with arguments" in {
    "List[Int]" shouldBeParsedAs RawQuery("List", List(RawQuery("Int")))
    "Map[Int, String]" shouldBeParsedAs RawQuery("Map", List(RawQuery("Int"), RawQuery("String")))
    "List[Option[String]]" shouldBeParsedAs RawQuery("List", List(RawQuery("Option", List(RawQuery("String")))))
  }

  it should "fail on empty argument lists" in {
    "List[]" shouldFail
  }

  it should "parse namespaces" in {
    "p.q.C" shouldBeParsedAs RawQuery("p.q.C")
    "p.q.C[argument.Name]" shouldBeParsedAs RawQuery("p.q.C", List(RawQuery("argument.Name")))
  }

  it should "parse type projections" in {
    "Outer#Inner" shouldBeParsedAs RawQuery("Outer#Inner")
    "Outer#Inner[A#B]" shouldBeParsedAs RawQuery("Outer#Inner", List(RawQuery("A#B")))
  }

  implicit class ParserResultExtensions(query: String) {
    lazy val res = QueryParser(query)

    def shouldBeParsedAs(expected: RawQuery) = {
      res should be('right)
      res.right.get should be(expected)
    }

    def shouldFail() = {
      res should be('left)
    }
  }
}

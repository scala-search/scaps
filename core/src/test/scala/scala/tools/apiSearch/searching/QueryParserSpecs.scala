package scala.tools.apiSearch.searching

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.language.postfixOps

class QueryParserSpecs extends FlatSpec with Matchers {
  import RawQuery._

  "the query parser" should "parse simple types" in {
    "Int" shouldBeParsedAs Type("Int")
  }

  it should "parse types with arguments" in {
    "List[Int]" shouldBeParsedAs Type("List", List(Type("Int")))
    "Map[Int, String]" shouldBeParsedAs Type("Map", List(Type("Int"), Type("String")))
    "List[Option[String]]" shouldBeParsedAs Type("List", List(Type("Option", List(Type("String")))))
  }

  it should "fail on empty argument lists" in {
    "List[]" shouldFail
  }

  it should "parse namespaces" in {
    "p.q.C" shouldBeParsedAs Type("p.q.C")
    "p.q.C[argument.Name]" shouldBeParsedAs Type("p.q.C", List(Type("argument.Name")))
  }

  it should "parse type projections" in {
    "Outer#Inner" shouldBeParsedAs Type("Outer#Inner")
    "Outer#Inner[A#B]" shouldBeParsedAs Type("Outer#Inner", List(Type("A#B")))
  }

  val A = Type("A")
  val B = Type("B")
  val C = Type("C")

  it should "parse function types" in {
    "A => B" shouldBeParsedAs function(A :: Nil, B)
  }

  it should "parse function types with multiple args" in {
    "(A, B) => C" shouldBeParsedAs function(A :: B :: Nil, C)
  }

  it should "follow precedence rules in nested function types" in {
    "A => B => C" shouldBeParsedAs function(A :: Nil, function(B :: Nil, C))
  }

  it should "parse tuple types" in {
    "(A, B)" shouldBeParsedAs tuple(A, B)
  }

  it should "allow tuples in function args" in {
    "((A, B)) => C" shouldBeParsedAs function(tuple(A, B) :: Nil, C)
  }

  it should "allow tuples in return types" in {
    "A => (B, C)" shouldBeParsedAs function(A :: Nil, tuple(B, C))
  }

  it should "fail on empty tuples" in {
    "()".shouldFail
    "A => ()".shouldFail
    "(())".shouldFail
  }

  it should "allow empty argument lists" in {
    "() => A" shouldBeParsedAs function(Nil, A)
  }

  implicit class ParserResultExtensions(query: String) {
    lazy val res = QueryParser(query)

    def shouldBeParsedAs(expected: Type) = {
      res should be('right)
      res.right.get.tpe should be(expected)
    }

    def shouldFail() = {
      res should be('left)
    }
  }
}

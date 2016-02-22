/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine.queries

import scala.language.postfixOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class QueryParserSpecs extends FlatSpec with Matchers {

  import RawQuery._

  "the query parser" should "parse simple types" in {
    parseFullQuery("Int").tpe should equal(Type("Int"))
  }

  it should "parse types with arguments" in {
    parseFullQuery("List[Int]").tpe should equal(Type("List", List(Type("Int"))))
    parseFullQuery("Map[Int, String]").tpe should equal(Type("Map", List(Type("Int"), Type("String"))))
    parseFullQuery("List[Option[String]]").tpe should equal(Type("List", List(Type("Option", List(Type("String"))))))
  }

  it should "fail on empty argument lists" in {
    failParse("List[]")
    ()
  }

  it should "parse namespaces" in {
    parseFullQuery("p.q.C").tpe should equal(Type("p.q.C"))
    parseFullQuery("p.q.C[argument.Name]").tpe should equal(Type("p.q.C", List(Type("argument.Name"))))
  }

  val A = Type("A")
  val B = Type("B")
  val C = Type("C")

  it should "parse function types" in {
    parseFullQuery("A => B").tpe should equal(function(A :: Nil, B))
  }

  it should "parse function types with multiple args" in {
    parseFullQuery("(A, B) => C").tpe should equal(function(A :: B :: Nil, C))
  }

  it should "follow precedence rules in nested function types" in {
    parseFullQuery("A => B => C").tpe should equal(function(A :: Nil, function(B :: Nil, C)))
  }

  it should "parse tuple types" in {
    parseFullQuery("(A, B)").tpe should equal(tuple(A, B))
  }

  it should "allow tuples in function args" in {
    parseFullQuery("((A, B)) => C").tpe should equal(function(tuple(A, B) :: Nil, C))
  }

  it should "allow tuples in return types" in {
    parseFullQuery("A => (B, C)").tpe should equal(function(A :: Nil, tuple(B, C)))
  }

  it should "not parse Tuple1 literals" in {
    parseFullQuery("(A)").tpe should equal(A)
  }

  it should "fail on empty tuples" in {
    failParse("()")
    failParse("A => ()")
    failParse("(())")
    ()
  }

  it should "fail on missing parens" in {
    val tpes = List(
      "(A, B",
      "A, B)",
      "(A, B => C",
      "A, B) => C",
      "A, B => C")

    tpes.flatMap(t => List(t, s"k1 k2: $t")).foreach { t =>
      failParse(t)
    }
  }

  it should "fail on missing brackets" in {
    val tpes = List(
      "List[A",
      "Map[A, List[C]")

    tpes.flatMap(t => List(t, s"k1 k2: $t")).foreach { t =>
      failParse(t)
    }
  }

  it should "allow empty argument lists" in {
    parseFullQuery("() => A").tpe should equal(function(Nil, A))
  }

  it should "parse single keywords preceding the type" in {
    parseFullQuery("keyword: Int").keywords should be("keyword")
  }

  it should "parse multiple keywords" in {
    parseFullQuery("k1 k2 k3: Int").keywords should be("k1 k2 k3")
  }

  it should "parse multiple keywords in quotes" in {
    parseFullQuery("\"k1 k2 k3\": Int").keywords should be("k1 k2 k3")
  }

  it should "allow colons in types" in {
    parseFullQuery("::[A, B]").tpe should equal(Type("::", A :: B :: Nil))
  }

  it should "parse multiple keywords without type" in {
    parseKeys("k1 k2 k3").keywords should be("k1 k2 k3")
  }

  def parseFullQuery(query: String) =
    parse(query) match {
      case f: RawQuery.Full => f
      case _                => ???
    }

  def parseKeys(query: String) =
    parse(query) match {
      case k: RawQuery.Keywords => k
      case _                    => ???
    }

  def parse(query: String) = {
    val res = QueryParser(query)
    res should be('right)
    res.getOrElse(???)
  }

  def failParse(query: String) = {
    val res = QueryParser(query)
    res should be('left)
    res.swap.getOrElse(???)
  }
}

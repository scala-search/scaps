package scaps.searchEngine.queries

import scaps.webapi.TypeRef
import scaps.searchEngine.SyntaxError
import scala.util.parsing.combinator.RegexParsers

import scalaz.\/
import scalaz.syntax.either.ToEitherOps

sealed trait RawQuery

object RawQuery {
  case class Keywords(keywords: String) extends RawQuery
  case class Full(keywords: String, tpe: RawQuery.Type) extends RawQuery

  case class Type(name: String, args: List[Type] = Nil)

  def function(args: List[Type], res: Type) =
    Type(TypeRef.Function.name(args.length), args :+ res)

  def tuple(tpes: Type*) =
    Type(TypeRef.Tuple.name(tpes.length), tpes.toList)
}

/**
 * Parses a String into a structured query.
 */
object QueryParser extends RegexParsers {
  import RawQuery._

  def query: Parser[RawQuery] = phrase(fullQuery) | phrase(tpeQuery) | keywordQuery

  def tpeQuery: Parser[RawQuery] = tpe ^^ { RawQuery.Full("", _) }

  def fullQuery: Parser[RawQuery] = (keywords <~ """\:\s""".r) ~ tpe ^^ {
    case keywords ~ tpe => RawQuery.Full(keywords, tpe)
  }

  def keywordQuery: Parser[RawQuery] = keywords ^^ {
    case keys => RawQuery.Keywords(keys)
  }

  def keywords: Parser[String] = quotedKeywords | unquotedKeywords

  def quotedKeywords: Parser[String] = "\"" ~> """[^\"]*""".r <~ "\""
  def unquotedKeywords: Parser[String] = """[^\:\.\,\[\]\(\)]*""".r

  def tpe: Parser[Type] = functionTpe | tupleTpe | simpleTpe

  def simpleTpe: Parser[Type] = name ~ opt("[" ~> rep1sep(tpe, ",") <~ "]") ^^ {
    case tpeName ~ Some(args) => Type(tpeName, args)
    case tpeName ~ None       => Type(tpeName)
  }

  def functionTpe: Parser[Type] = (functionArgs <~ "=>") ~ tpe ^^ {
    case args ~ returnTpe => function(args, returnTpe)
  }

  def functionArgs: Parser[List[Type]] = simpleTpe ^^ { tpe => List(tpe) } |
    "(" ~> repsep(tpe, ",") <~ ")"

  def tupleTpe: Parser[Type] = "(" ~> rep1sep(tpe, ",") <~ ")" ^^ {
    case tpe :: Nil => tpe
    case tpes       => tuple(tpes: _*)
  }

  def name: Parser[String] = identifier ~ rep("""[\.]""".r ~ identifier) ^^ {
    case head ~ rest => head + rest.map { case sep ~ ident => s"$sep$ident" }.mkString("")
  }

  /**
   * A Scala identifier.
   *
   * This does not really implement Scala's identifier but suffices to distinguish them from whitespaces, brackets and
   * namespace delimiters.
   */
  def identifier: Parser[String] = """[^\.\,\[\]\s\(\)]+""".r

  def apply(input: String): SyntaxError \/ RawQuery =
    parseAll(query, input) match {
      case Success(result, _) => result.right
      case NoSuccess(msg, _)  => SyntaxError(msg).left
    }
}

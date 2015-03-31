package scala.tools.apiSearch.searching

import scala.util.parsing.combinator.RegexParsers
import scala.tools.apiSearch.model.ClassEntity
import scala.tools.apiSearch.model.TypeEntity

case class RawQuery(keywords: List[String], tpe: RawQuery.Type)

object RawQuery {
  case class Type(name: String, args: List[Type] = Nil)

  def function(args: List[Type], res: Type) =
    Type(TypeEntity.functionType(args.length), args :+ res)

  def tuple(tpes: Type*) =
    Type(TypeEntity.tupleType(tpes.length), tpes.toList)
}

/**
 * Parses a String into a structured query.
 */
object QueryParser extends RegexParsers {
  import RawQuery._

  def query: Parser[RawQuery] = keywords ~ tpe ^^ {
    case keywords ~ tpe => RawQuery(keywords, tpe)
  }

  def keywords: Parser[List[String]] = opt((escapedKeywords | singleKeyword) <~ ":") ^^ { _.getOrElse(Nil) }

  def singleKeyword: Parser[List[String]] = """[^`\s\:]*""".r ^^ { _ :: Nil }

  def escapedKeywords: Parser[List[String]] = "`" ~> rep("""[^`\s]+""".r) <~ "`"

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

  def name: Parser[String] = identifier ~ rep("""[\.\#]""".r ~ identifier) ^^ {
    case head ~ rest => head + rest.map { case sep ~ ident => s"$sep$ident" }.mkString("")
  }

  /**
   * A Scala identifier.
   *
   * This does not really implement Scala's identifier but suffices to distinguish them from whitespaces, brackets and
   * namespace delimiters.
   */
  def identifier: Parser[String] = """[^\.\#\,\[\]\s\(\)`]+""".r

  def apply(input: String): Either[String, RawQuery] =
    parseAll(query, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _)  => Left(msg)
    }
}

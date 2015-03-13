package scala.tools.apiSearch.searching

import scala.util.parsing.combinator.RegexParsers

case class RawQuery(tpe: String, args: List[RawQuery] = Nil)

/**
 * Parses a String into a structured query.
 */
object QueryParser extends RegexParsers {
  def query: Parser[RawQuery] = tpe

  def tpe: Parser[RawQuery] = name ~ opt("[" ~> rep1sep(tpe, ",") <~ "]") ^^ {
    case tpeName ~ Some(args) => RawQuery(tpeName, args)
    case tpeName ~ None       => RawQuery(tpeName)
  }

  def name: Parser[String] = identifier ~ rep("""[\.#]""".r ~ identifier) ^^ {
    case head ~ rest => head + rest.map { case sep ~ ident => s"$sep$ident" }.mkString("")
  }

  /**
   * A Scala identifier.
   *
   * This does not really implement Scala's identifier but suffices to distinguish them from brackets and
   * namespace delimiters.
   */
  def identifier: Parser[String] = """[^\.\#\,\[\]\s]+""".r

  def apply(input: String): Either[String, RawQuery] =
    parseAll(query, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _)  => Left(msg)
    }
}

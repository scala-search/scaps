package scaps.searchEngine

import scaps.webapi.Variance

case class ApiQuery(keywords: List[String], types: List[ApiQuery.Type]) {
  lazy val allAlternatives = types.flatMap(_.alternatives)

  override def toString =
    s"${keywords.mkString(" ")}: ${types.mkString(" ")}"
}

object ApiQuery {
  case class Type(alternatives: List[Alternative]) {
    override def toString =
      alternatives.map(_.toString).mkString("(", " ", ")")
  }

  case class Alternative(variance: Variance, typeName: String, occurrence: Int, boost: Double) {
    override def toString =
      s"${variance.prefix}${typeName}_$occurrence^${(boost * 100).round.toFloat / 100}"
  }
}

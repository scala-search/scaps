package scaps.searchEngine

import scaps.webapi.Variance

case class ApiQuery(keywords: List[String], tpe: ApiTypeQuery) {
  override def toString =
    s"${keywords.mkString(" ")}: $tpe"

  def allTypes = tpe.allTypes
}

sealed trait ApiTypeQuery {
  import ApiTypeQuery._

  def children: List[ApiTypeQuery]

  def allTypes: List[Type] = this match {
    case t: Type => List(t)
    case _       => children.flatMap(_.allTypes)
  }
}

object ApiTypeQuery {
  case class Sum(parts: List[ApiTypeQuery]) extends ApiTypeQuery {
    def children = parts

    override def toString =
      parts.mkString("sum(", ", ", ")")
  }

  case class Max(alternatives: List[ApiTypeQuery]) extends ApiTypeQuery {
    def children = alternatives

    override def toString =
      alternatives.mkString("max(", ", ", ")")
  }

  case class Type(variance: Variance, typeName: String, boost: Double) extends ApiTypeQuery {
    def children = Nil

    override def toString =
      s"${variance.prefix}${typeName}^$boost"
  }
}

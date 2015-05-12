package scaps.searchEngine

import scaps.webapi.Variance

case class APIQuery(keywords: List[String], types: List[APIQuery.Type]) {
  def fingerprint: List[String] =
    for {
      tpe <- types
    } yield s"${tpe.variance.prefix}${tpe.typeName}_${tpe.occurrence}"

  override def toString =
    s"${keywords.mkString(" ")}: ${types.mkString(" ")}"
}

object APIQuery {
  case class Type(variance: Variance, typeName: String, occurrence: Int, boost: Float) {
    override def toString =
      s"${variance.prefix}${typeName}_$occurrence^${(boost * 100).round.toFloat / 100}"
  }
}

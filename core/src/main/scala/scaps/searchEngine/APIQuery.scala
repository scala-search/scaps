package scaps.searchEngine

import scaps.webapi.Variance

case class APIQuery(keywords: List[String], types: List[APIQuery.Type]) {
  def fingerprint: List[String] =
    for {
      tpe <- types
    } yield s"${tpe.variance.prefix}${tpe.typeName}_${tpe.occurrence}"
}

object APIQuery {
  case class Type(variance: Variance, typeName: String, occurrence: Int, boost: Float)
}
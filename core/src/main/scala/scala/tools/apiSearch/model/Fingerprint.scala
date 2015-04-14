package scala.tools.apiSearch.model

case class Fingerprint(types: List[Fingerprint.Type]) {
  import Fingerprint._

  override def toString =
    types.groupBy(identity)
      .flatMap { case (tpe, values) => values.zipWithIndex.map { case (_, idx) => s"${tpe.variance.prefix}${tpe.name}_${idx}" } }
      .mkString(" ")
}

object Fingerprint {
  case class Type(variance: Variance, name: String)
}

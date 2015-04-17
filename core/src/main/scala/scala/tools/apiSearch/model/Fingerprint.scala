package scala.tools.apiSearch.model

case class Fingerprint(types: List[Fingerprint.Type]) {
  import Fingerprint._

  def typesWithOccurrenceIndex(implicit o: Ordering[Fingerprint.Type]): List[(Type, Int)] =
    types.groupBy(fpt => (fpt.variance, fpt.name))
      .toList
      .flatMap { case (tpe, values) => values.sorted.zipWithIndex }

  override def toString =
    typesWithOccurrenceIndex(Ordering[Int].on(-_.depth))
      .map {
        case (tpe, idx) => {
          val depthdist =
            if (tpe.distance == 0) s"${tpe.depth}"
            else s"(${tpe.depth}, ${tpe.distance})"
          s"${tpe.variance.prefix}${tpe.name}_${idx}^$depthdist"
        }
      }
      .mkString(" ")

  def bagOfTypes =
    typesWithOccurrenceIndex(Ordering[Int].on(-_.depth))
      .map {
        case (tpe, idx) => {
          s"${tpe.variance.prefix}${tpe.name}_${idx}"
        }
      }
}

object Fingerprint {
  case class Type(variance: Variance, name: String, depth: Int, distance: Int = 0)
}

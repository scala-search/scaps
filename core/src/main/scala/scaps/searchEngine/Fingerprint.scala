package scaps.searchEngine

import scaps.webapi.Variance
import scaps.webapi.TermEntity
import scaps.webapi.TypeEntity
import scala.Ordering

case class Fingerprint(types: List[Fingerprint.Type]) {
  import Fingerprint._

  def typesWithOccurrenceIndex(implicit o: Ordering[Fingerprint.Type]): List[(Type, Int)] =
    types.groupBy(fpt => (fpt.variance, fpt.name))
      .toList
      .flatMap { case (tpe, values) => values.sorted.zipWithIndex }

  override def toString =
    bagOfTypes.mkString(" ")

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

  def apply(term: TermEntity): Fingerprint =
    Fingerprint(term.tpe.normalize(term.typeParameters))

  def apply(tpe: TypeEntity): Fingerprint =
    Fingerprint(fingerprintTypes(tpe))

  def fingerprintTypes(tpe: TypeEntity, depth: Int = 0): List[Fingerprint.Type] =
    tpe match {
      case TypeEntity.Ignored(args, _) =>
        args.flatMap(fingerprintTypes(_, depth + 1))
      case tpe =>
        Fingerprint.Type(tpe.variance, tpe.name, depth) :: tpe.args.flatMap(fingerprintTypes(_, depth + 1))
    }
}

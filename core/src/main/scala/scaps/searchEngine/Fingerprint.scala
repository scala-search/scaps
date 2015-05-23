package scaps.searchEngine

import scala.Ordering
import scaps.webapi._

case class Fingerprint(types: List[Fingerprint.Type]) {
  import Fingerprint._

  def typesWithOccurrenceIndex(): List[(Type, Int)] =
    types.groupBy(fpt => (fpt.variance, fpt.name))
      .toList
      .flatMap { case (_, values) => values.zipWithIndex }

  override def toString =
    typesWithOccurrenceIndex()
      .map {
        case (tpe, idx) => {
          s"${tpe.variance.prefix}${tpe.name}_${idx}"
        }
      }.mkString(" ")
}

object Fingerprint {
  case class Type(variance: Variance, name: String, depth: Int)

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

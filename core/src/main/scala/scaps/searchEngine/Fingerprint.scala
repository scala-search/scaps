package scaps.searchEngine

import scala.Ordering
import scaps.webapi._

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
  case class Type(variance: Variance, name: String, depth: Int, distance: Double = 0) {
    assert(distance >= 0d && distance <= 1d, s"distance $distance of ${variance.prefix}$name was not between 0 and 1")
  }

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

  def queryFingerprint(findClass: String => Option[ClassEntity], findSubClasses: TypeEntity => Seq[ClassEntity],
                       term: TermEntity): Fingerprint =
    queryFingerprint(findClass, findSubClasses, term.tpe.normalize(term.typeParameters))

  def queryFingerprint(findClass: String => Option[ClassEntity], findSubClasses: TypeEntity => Seq[ClassEntity],
                       tpe: TypeEntity): Fingerprint = {
    def fingerprintWithAlternatives(tpe: TypeEntity, depth: Int): List[Type] =
      tpe match {
        case TypeEntity.Ignored(args, _) =>
          args.flatMap(fingerprintWithAlternatives(_, depth + 1))
        case tpe: TypeEntity =>
          val thisFpt = Fingerprint.Type(tpe.variance, tpe.name, depth)

          val alternatives = tpe.variance match {
            case Covariant =>
              val subTypesWithDist = findSubClasses(tpe).toList
                .map(subCls => (thisFpt.copy(name = subCls.name), subCls.baseTypes.indexWhere(_.name == tpe.name)))

              val maxDist = (1 :: subTypesWithDist.map(_._2)).max

              val subTypes = subTypesWithDist.map {
                case (subTpe, dist) => subTpe.copy(distance = dist.toDouble / maxDist)
              }

              subTypes :+ thisFpt.copy(name = TypeEntity.Nothing.name, distance = 1)
            case Contravariant =>
              (for {
                cls <- findClass(tpe.name).toSeq
                maxDist = cls.baseTypes.length
                baseTpe <- cls.baseTypes
              } yield {
                val dist = cls.baseTypes.indexOf(baseTpe)
                thisFpt.copy(name = baseTpe.name, distance = dist.toDouble / maxDist)
              }).toList
            case Invariant if tpe.name != TypeEntity.Unknown.name =>
              thisFpt.copy(name = TypeEntity.Unknown.name, distance = 1) :: Nil
            case Invariant =>
              Nil
          }

          thisFpt :: alternatives ::: tpe.args.flatMap(arg => fingerprintWithAlternatives(arg, depth + 1))
      }

    Fingerprint(fingerprintWithAlternatives(tpe, 0))
  }
}

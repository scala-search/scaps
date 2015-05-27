package scaps.searchEngine

import scaps.webapi._

case class QueryFingerprint(types: List[QueryFingerprint.Type])

object QueryFingerprint {
  case class Type(variance: Variance, alternatives: List[Alternative], depth: Int)

  case class Alternative(typeName: String, distance: Int)

  def apply(findAlternativesWithDistance: TypeEntity => Seq[(TypeEntity, Int)], term: TermEntity): QueryFingerprint =
    apply(findAlternativesWithDistance, term.tpe.normalize(term.typeParameters))

  def apply(findAlternativesWithDistance: TypeEntity => Seq[(TypeEntity, Int)], tpe: TypeEntity): QueryFingerprint = {
    def fingerprintWithAlternatives(tpe: TypeEntity, depth: Int): List[Type] =
      tpe match {
        case TypeEntity.Ignored(args, _) =>
          args.flatMap(fingerprintWithAlternatives(_, depth + 1))
        case tpe: TypeEntity =>
          val thisTpe = Alternative(tpe.name, 0)

          val alternatives = findAlternativesWithDistance(tpe)
            .map { case (altTpe, dist) => Alternative(altTpe.name, dist) }

          Type(tpe.variance, thisTpe :: alternatives.toList, depth) ::
            tpe.args.flatMap(arg => fingerprintWithAlternatives(arg, depth + 1))
      }

    QueryFingerprint(fingerprintWithAlternatives(tpe, 0))
  }
}

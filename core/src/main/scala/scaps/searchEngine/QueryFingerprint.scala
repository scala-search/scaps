package scaps.searchEngine

import scaps.webapi._

case class QueryFingerprint(types: List[QueryFingerprint.Type])

object QueryFingerprint {
  case class Type(variance: Variance, alternatives: List[Alternative], depth: Int)

  case class Alternative(typeName: String, distance: Int)

  def apply(findView: TypeEntity => Seq[View], term: TermEntity): QueryFingerprint =
    apply(findView, term.tpe.normalize(term.typeParameters))

  def apply(findView: TypeEntity => Seq[View], tpe: TypeEntity): QueryFingerprint = {
    def fingerprintWithAlternatives(tpe: TypeEntity, depth: Int): List[Type] =
      tpe match {
        case TypeEntity.Ignored(args, _) =>
          args.flatMap(fingerprintWithAlternatives(_, depth + 1))
        case tpe: TypeEntity =>
          val thisTpe = Alternative(tpe.name, 0)

          val views = findView(tpe)
          val maxDistance = (0 +: views.flatMap(_.distance)).max
          val alternatives = views.map(view =>
            Alternative(view.otherEnd(tpe).name, (view.relativeDistance(maxDistance) * 10).toInt))

          Type(tpe.variance, thisTpe :: alternatives.toList, depth) ::
            tpe.args.flatMap(arg => fingerprintWithAlternatives(arg, depth + 1))
      }

    QueryFingerprint(fingerprintWithAlternatives(tpe, 0))
  }
}

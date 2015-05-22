package scaps.searchEngine

import scaps.webapi._

case class QueryFingerprint(types: List[QueryFingerprint.Type])

object QueryFingerprint {
  case class Type(variance: Variance, alternatives: List[Alternative], depth: Int)

  case class Alternative(typeName: String, distance: Int)

  def apply(findClass: String => Option[ClassEntity], findSubClasses: TypeEntity => Seq[ClassEntity],
            term: TermEntity): QueryFingerprint =
    apply(findClass, findSubClasses, term.tpe.normalize(term.typeParameters))

  def apply(findClass: String => Option[ClassEntity], findSubClasses: TypeEntity => Seq[ClassEntity],
            tpe: TypeEntity): QueryFingerprint = {
    def fingerprintWithAlternatives(tpe: TypeEntity, depth: Int): List[Type] =
      tpe match {
        case TypeEntity.Ignored(args, _) =>
          args.flatMap(fingerprintWithAlternatives(_, depth + 1))
        case tpe: TypeEntity =>
          val thisTpe = Alternative(tpe.name, 0)

          val alternatives = tpe.variance match {
            case Covariant =>
              val subTypesWithDist = findSubClasses(tpe).toList
                .map(subCls => Alternative(subCls.name, subCls.baseTypes.indexWhere(_.name == tpe.name)))

              val maxDist = (1 :: subTypesWithDist.map(_.distance)).max

              Alternative(TypeEntity.Nothing.name, maxDist + 1) :: subTypesWithDist
            case Contravariant =>
              (for {
                cls <- findClass(tpe.name).toSeq
                baseTpe <- cls.baseTypes
              } yield {
                val dist = cls.baseTypes.indexOf(baseTpe)
                Alternative(baseTpe.name, dist)
              }).toList
            case Invariant if tpe.name != TypeEntity.Unknown.name =>
              Alternative(TypeEntity.Unknown.name, 1) :: Nil
            case Invariant =>
              Nil
          }

          Type(tpe.variance, thisTpe :: alternatives, depth) ::
            tpe.args.flatMap(arg => fingerprintWithAlternatives(arg, depth + 1))
      }

    QueryFingerprint(fingerprintWithAlternatives(tpe, 0))
  }
}

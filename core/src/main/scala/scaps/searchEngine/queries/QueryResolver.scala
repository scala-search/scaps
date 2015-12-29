package scaps.searchEngine.queries

import scalaz.{ Contravariant => _, _ }
import scalaz.Scalaz._
import scaps.searchEngine.SemanticError
import scaps.searchEngine.NameNotFound
import scaps.searchEngine.UnexpectedNumberOfTypeArgs
import scaps.searchEngine.NameAmbiguous
import scaps.api._

private[queries] sealed trait ResolvedQuery
private[queries] object ResolvedQuery {
  case object Wildcard extends ResolvedQuery
  case class Type(cls: TypeDef, args: List[ResolvedQuery]) extends ResolvedQuery
}

/**
 * Resolves all type names in the query and assigns the according class entities.
 *
 * Names that cannot be resolved and have length 1 are treated as type parameters.
 */
class QueryResolver(findTypeDefsBySuffix: (String) => Seq[TypeDef]) {
  def apply(raw: RawQuery.Type): SemanticError \/ TypeRef =
    for {
      resolved <- resolveNames(raw)
    } yield toType(resolved)

  private def resolveNames(raw: RawQuery.Type): SemanticError \/ ResolvedQuery = {
    val resolvedArgs: SemanticError \/ List[ResolvedQuery] =
      raw.args.map(arg => resolveNames(arg)).sequenceU

    def isTypeParam(name: String): Boolean =
      name.length() == 1 && (name(0).isLetter || name(0) == '_')

    resolvedArgs.flatMap { resolvedArgs =>
      findTypeDefsBySuffix(raw.name) match {
        case Seq() if isTypeParam(raw.name) =>
          \/.right(ResolvedQuery.Wildcard)
        case Seq() =>
          \/.left(NameNotFound(raw.name))
        case Seq(cls) if resolvedArgs.length == cls.typeParameters.length =>
          \/.right(ResolvedQuery.Type(cls, resolvedArgs))
        case Seq(cls) if raw.args.length == 0 =>
          \/.right(ResolvedQuery.Type(cls, cls.typeParameters.map(_ => ResolvedQuery.Wildcard)))
        case Seq(cls) =>
          \/.left(UnexpectedNumberOfTypeArgs(raw.name, cls.typeParameters.length))
        case candidates =>
          \/.left(NameAmbiguous(raw.name, candidates))
      }
    }
  }

  private def toType(resolved: ResolvedQuery): TypeRef = {
    def rec(resolved: ResolvedQuery, variance: Variance): TypeRef =
      resolved match {
        case ResolvedQuery.Wildcard =>
          TypeRef.Unknown(variance)
        case ResolvedQuery.Type(cls, args) =>
          val tpeArgs = cls.typeParameters.zip(args).map {
            case (tpeParam, ResolvedQuery.Wildcard) =>
              (variance * tpeParam.variance) match {
                case Covariant     => tpeParam.lowerBound
                case Contravariant => tpeParam.upperBound
                case Invariant     => TypeRef.Unknown(Invariant)
              }
            case (tpeParam, arg) =>
              rec(arg, variance * tpeParam.variance)
          }
          TypeRef(cls.name, variance, tpeArgs)
      }

    rec(resolved, Covariant)
  }
}

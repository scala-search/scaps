package scaps.searchEngine.queries

import scala.Ordering
import scaps.webapi.ClassEntity
import scaps.webapi.Contravariant
import scaps.webapi.Covariant
import scaps.webapi.TypeEntity
import scaps.webapi.Variance
import scaps.searchEngine.APIQuery
import scaps.searchEngine.NameAmbiguous
import scaps.searchEngine.NameNotFound
import scaps.searchEngine.SemanticError
import scaps.searchEngine.UnexpectedNumberOfTypeArgs
import scaps.settings.QuerySettings
import scala.util.Try
import scalaz.{ \/ => \/ }
import scalaz.std.list.listInstance
import scalaz.syntax.either.ToEitherOps
import scalaz.syntax.traverse.ToTraverseOps
import scaps.webapi.Invariant
import scaps.webapi.ClassEntity
import scaps.searchEngine.Fingerprint

private[queries] sealed trait ResolvedQuery
private[queries] object ResolvedQuery {
  case object Wildcard extends ResolvedQuery
  case class Type(cls: ClassEntity, args: List[ResolvedQuery]) extends ResolvedQuery
}

/**
 * Analyzes parsed queries.
 *
 * Instances require access to the class index which can be injected via
 * `findClassesBySuffix` and `findSubClasses`
 */
class QueryAnalyzer private[searchEngine] (
  settings: QuerySettings,
  findClassesBySuffix: (String) => Seq[ClassEntity],
  findSubClasses: (TypeEntity) => Seq[ClassEntity]) {

  /**
   * Transforms a parsed query into a query that can be passed to the terms index.
   *
   * Fails when `findClassesBySuffix` or `findSubClasses` fails.
   */
  def apply(raw: RawQuery): SemanticError \/ APIQuery =
    resolveNames(raw.tpe).map(
      (toType _) andThen
        (_.normalize(Nil)) andThen
        (tpe => Fingerprint(fingerprintWithAlternatives(tpe))) andThen
        (toApiQuery _) andThen
        { apiQuery => apiQuery.copy(keywords = raw.keywords) })

  /**
   * Resolves all type names in the query and assigns the according class entities.
   *
   * Names that cannot be resolved and have length 1 are treated as type parameters.
   */
  private def resolveNames(raw: RawQuery.Type): SemanticError \/ ResolvedQuery = {
    val resolvedArgs: SemanticError \/ List[ResolvedQuery] =
      raw.args.map(arg => resolveNames(arg)).sequenceU

    resolvedArgs.flatMap { resolvedArgs =>
      findClassesBySuffix(raw.name) match {
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

  private def isTypeParam(name: String): Boolean =
    name.length() == 1

  private def toType(resolved: ResolvedQuery): TypeEntity = {
    def rec(resolved: ResolvedQuery, variance: Variance): TypeEntity =
      resolved match {
        case ResolvedQuery.Wildcard =>
          TypeEntity.Unknown(variance)
        case ResolvedQuery.Type(cls, args) =>
          val tpeArgs = cls.typeParameters.zip(args).map {
            case (tpeParam, ResolvedQuery.Wildcard) =>
              (variance * tpeParam.variance) match {
                case Covariant     => TypeEntity(tpeParam.lowerBound, Covariant, Nil)
                case Contravariant => TypeEntity(tpeParam.upperBound, Contravariant, Nil)
                case Invariant     => TypeEntity.Unknown(Invariant)
              }
            case (tpeParam, arg) =>
              rec(arg, variance * tpeParam.variance)
          }
          TypeEntity(cls.name, variance, tpeArgs)
      }

    rec(resolved, Covariant)
  }

  private def fingerprintWithAlternatives(tpe: TypeEntity, depth: Int = 0): List[Fingerprint.Type] =
    tpe match {
      case TypeEntity.Ignored(args, _) =>
        args.flatMap(fingerprintWithAlternatives(_, depth + 1))
      case tpe: TypeEntity =>
        val thisFpt = Fingerprint.Type(tpe.variance, tpe.name, depth, 0)

        val alternatives = tpe.variance match {
          case Covariant =>
            val subTypes = findSubClasses(tpe).toList
              .map(subCls => thisFpt.copy(name = subCls.name, distance = subCls.baseTypes.indexWhere(_.name == tpe.name) + 1))

            subTypes :+ thisFpt.copy(name = TypeEntity.Nothing.name, distance = (0 :: subTypes.map(_.distance)).max + 1)
          case Contravariant =>
            findClassesBySuffix(tpe.name).headOption.toList
              .flatMap(cls => cls.baseTypes.zipWithIndex.map { case (baseCls, idx) => thisFpt.copy(name = baseCls.name, distance = idx + 1) })
          case Invariant if tpe.name != TypeEntity.Unknown.name =>
            thisFpt.copy(name = TypeEntity.Unknown.name, distance = 1) :: Nil
          case Invariant =>
            Nil
        }

        thisFpt :: alternatives ::: tpe.args.flatMap(arg => fingerprintWithAlternatives(arg, depth + 1))
    }

  private def toApiQuery(fingerprint: Fingerprint): APIQuery = {
    val tpes = fingerprint
      .typesWithOccurrenceIndex(Ordering[Float].on(fpt => -boost(fpt)))
      .map {
        case (tpe, idx) =>
          APIQuery.Type(tpe.variance, tpe.name, idx, boost(tpe))
      }

    APIQuery(Nil, tpes.toList.sortBy(-_.boost))
  }

  private def boost(tpe: Fingerprint.Type): Float =
    distanceBoost(tpe.distance) * depthBoost(tpe.depth)

  private def distanceBoost(dist: Int): Float = adjust(1d / (dist + 1), settings.distanceBoostGradient)
  private def depthBoost(depth: Int): Float = adjust(1d / (depth + 1), settings.depthBoostGradient)

  private def adjust(x: Double, factor: Double): Float = (((x - 1) * factor) + 1).toFloat
}

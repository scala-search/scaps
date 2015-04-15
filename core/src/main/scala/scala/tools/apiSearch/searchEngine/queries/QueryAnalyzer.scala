package scala.tools.apiSearch.searchEngine.queries

import scala.Ordering
import scala.tools.apiSearch.model.ClassEntity
import scala.tools.apiSearch.model.Contravariant
import scala.tools.apiSearch.model.Covariant
import scala.tools.apiSearch.model.Fingerprint
import scala.tools.apiSearch.model.TypeEntity
import scala.tools.apiSearch.model.Variance
import scala.tools.apiSearch.searchEngine.APIQuery
import scala.tools.apiSearch.searchEngine.NameAmbiguous
import scala.tools.apiSearch.searchEngine.NameNotFound
import scala.tools.apiSearch.searchEngine.SemanticError
import scala.tools.apiSearch.searchEngine.UnexpectedNumberOfTypeArgs
import scala.tools.apiSearch.settings.QuerySettings
import scala.util.Try
import scalaz.{ \/ => \/ }
import scalaz.std.list.listInstance
import scalaz.syntax.either.ToEitherOps
import scalaz.syntax.traverse.ToTraverseOps
import scala.tools.apiSearch.model.Invariant

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
  findClassesBySuffix: (String) => Try[Seq[ClassEntity]],
  findSubClasses: (String) => Try[Seq[ClassEntity]]) {

  /**
   * Transforms a parsed query into a query that can be passed to the terms index.
   *
   * Fails when `findClassesBySuffix` or `findSubClasses` fails.
   */
  def apply(raw: RawQuery): Try[SemanticError \/ APIQuery] =
    Try {
      resolveNames(raw.tpe).get.map(
        (toType _) andThen
          (_.normalize(Nil)) andThen
          (tpe => Fingerprint(fingerprintWithAlternatives(tpe).get)) andThen
          (toApiQuery _) andThen
          { apiQuery => apiQuery.copy(keywords = raw.keywords) })
    }

  /**
   * Resolves all type names in the query and assigns the according class entities.
   *
   * Names that cannot be resolved and have length 1 are treated as type parameters.
   */
  private def resolveNames(raw: RawQuery.Type): Try[SemanticError \/ ResolvedQuery] =
    Try {
      val resolvedArgs: SemanticError \/ List[ResolvedQuery] =
        raw.args.map(arg => resolveNames(arg).get).sequenceU

      resolvedArgs.flatMap { resolvedArgs =>
        findClassesBySuffix(raw.name).get match {
          case Seq() if isTypeParam(raw.name) =>
            ResolvedQuery.Wildcard.right
          case Seq() =>
            NameNotFound(raw.name).left
          case Seq(cls) if resolvedArgs.length == cls.typeParameters.length || raw.args.length == 0 =>
            ResolvedQuery.Type(cls, resolvedArgs).right
          case Seq(cls) =>
            UnexpectedNumberOfTypeArgs(raw.name, cls.typeParameters.length).left
          case candidates =>
            NameAmbiguous(raw.name, candidates).left
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
            case (tpeParam, arg) => rec(arg, variance * tpeParam.variance)
          }
          TypeEntity(cls.name, variance, tpeArgs)
      }

    rec(resolved, Covariant)
  }

  private def fingerprintWithAlternatives(tpe: TypeEntity, depth: Int = 0): Try[List[Fingerprint.Type]] =
    Try {
      tpe match {
        case TypeEntity.Unknown(_) =>
          Nil
        case tpe: TypeEntity =>
          val thisFpt = Fingerprint.Type(tpe.variance, tpe.name, depth, 0)

          val alternatives = tpe.variance match {
            case Covariant =>
              findSubClasses(tpe.name).get.toList
                .map(subCls => thisFpt.copy(name = subCls.name, distance = subCls.baseTypes.indexWhere(_.name == tpe.name) + 1))
            case Contravariant =>
              findClassesBySuffix(tpe.name).get.headOption.toList
                .flatMap(cls => cls.baseTypes.zipWithIndex.map { case (baseCls, idx) => thisFpt.copy(name = baseCls.name, distance = idx + 1) })
            case Invariant => Nil
          }

          thisFpt :: alternatives ::: tpe.args.flatMap(arg => fingerprintWithAlternatives(arg, depth + 1).get)
      }
    }

  private def toApiQuery(fingerprint: Fingerprint): APIQuery = {
    val tpes = fingerprint.typesWithOccurrenceIndex(Ordering[Float].on(fpt => -boost(fpt))).map {
      case (tpe, idx) => APIQuery.Type(tpe.variance, tpe.name, idx, boost(tpe))
    }

    APIQuery(Nil, tpes.toList.sortBy(-_.boost))
  }

  private def boost(tpe: Fingerprint.Type): Float =
    distanceBoost(tpe.distance) * depthBoost(tpe.depth)

  private def distanceBoost(dist: Int): Float = (1d / (settings.distanceBoostGradient * dist + 1d)).toFloat
  private def depthBoost(depth: Int): Float = (1d / (settings.depthBoostGradient * depth + 1d)).toFloat
}

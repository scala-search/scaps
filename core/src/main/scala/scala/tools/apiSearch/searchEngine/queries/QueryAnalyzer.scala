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

private[queries] sealed trait ResolvedQuery
private[queries] object ResolvedQuery {
  case object Wildcard extends ResolvedQuery
  case class Type(cls: ClassEntity, args: List[ResolvedQuery]) extends ResolvedQuery
}

private[queries] case class FlattenedQuery(types: List[List[FlattenedQuery.Type]])
private[queries] object FlattenedQuery {
  case class Type(variance: Variance, typeName: String, distance: Int, depth: Int)
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
          (tpe => Fingerprint(tpe.fingerprintTypes())) andThen
          (fp => getAlternatives(fp).get) andThen
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

  private def getAlternatives(fingerprint: Fingerprint): Try[Fingerprint] =
    Try {
      val alternatives = fingerprint.types.flatMap {
        case Fingerprint.Type(Covariant, tpeName, depth, _) =>
          (findSubClasses(tpeName).get)
            .map(subCls => Fingerprint.Type(Covariant, subCls.name, depth, subCls.baseTypes.indexWhere(_.name == tpeName) + 1))
        case Fingerprint.Type(Contravariant, tpeName, depth, _) =>
          (findClassesBySuffix(tpeName).get).headOption.toList
            .flatMap(cls => cls.baseTypes.zipWithIndex.map { case (baseCls, idx) => Fingerprint.Type(Covariant, baseCls.name, depth, idx + 1) })
        case _ => Nil
      }

      fingerprint.copy(types = fingerprint.types ::: alternatives)
    }

  private def toApiQuery(fingerprint: Fingerprint): APIQuery = {
    val tpes = fingerprint.typesWithOccurrenceIndex(Ordering[Float].on(fpt => boost(fpt))).map {
      case (tpe, idx) => APIQuery.Type(tpe.variance, tpe.name, idx, boost(tpe))
    }

    APIQuery(Nil, tpes.toList.sortBy(-_.boost))
  }

  private def boost(tpe: Fingerprint.Type): Float =
    distanceBoost(tpe.distance) * depthBoost(tpe.depth)

  private def distanceBoost(dist: Int): Float = (1d / (settings.distanceBoostGradient * dist + 1d)).toFloat
  private def depthBoost(depth: Int): Float = (1d / (settings.depthBoostGradient * depth + 1d)).toFloat
}

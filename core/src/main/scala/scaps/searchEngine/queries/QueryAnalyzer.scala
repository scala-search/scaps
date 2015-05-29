package scaps.searchEngine.queries

import scalaz.{ \/ => \/ }
import scalaz.std.list.listInstance
import scalaz.syntax.traverse.ToTraverseOps
import scaps.searchEngine.ApiQuery
import scaps.searchEngine.NameAmbiguous
import scaps.searchEngine.NameNotFound
import scaps.searchEngine.QueryFingerprint
import scaps.searchEngine.SemanticError
import scaps.searchEngine.UnexpectedNumberOfTypeArgs
import scaps.settings.Settings
import scaps.webapi.ClassEntity
import scaps.webapi.Contravariant
import scaps.webapi.Covariant
import scaps.webapi.Invariant
import scaps.webapi.TypeEntity
import scaps.webapi.Variance
import scaps.searchEngine.View

private[queries] sealed trait ResolvedQuery
private[queries] object ResolvedQuery {
  case object Wildcard extends ResolvedQuery
  case class Type(cls: ClassEntity, args: List[ResolvedQuery]) extends ResolvedQuery
}

sealed trait ExpandedQuery {
  def children: List[ExpandedQuery]
  def withChildren(children: List[ExpandedQuery]): ExpandedQuery
}

private[queries] object ExpandedQuery {
  case class Sum(parts: List[ExpandedQuery]) extends ExpandedQuery {
    def children = parts
    def withChildren(children: List[ExpandedQuery]) =
      copy(parts = children)

    override def toString =
      parts.mkString("sum(", ", ", ")")
  }
  object Sum {
    def apply(parts: ExpandedQuery*): Sum =
      Sum(parts.toList)
  }

  case class Max(alternatives: List[ExpandedQuery]) extends ExpandedQuery {
    def children = alternatives
    def withChildren(children: List[ExpandedQuery]) =
      copy(alternatives = children)

    override def toString =
      alternatives.mkString("max(", ", ", ")")
  }
  object Max {
    def apply(alts: ExpandedQuery*): Max =
      Max(alts.toList)
  }

  case class Leaf(tpe: TypeEntity, depth: Int, dist: Int) extends ExpandedQuery {
    def children = Nil
    def withChildren(children: List[ExpandedQuery]) =
      this

    override def toString =
      s"$tpe^($depth, $dist)"
  }
}

/**
 * Analyzes parsed queries.
 *
 * Instances require access to the class index which can be injected via
 * `findClassesBySuffix` and `findSubClasses`
 */
class QueryAnalyzer private[searchEngine] (
  settings: Settings,
  findClassesBySuffix: (String) => Seq[ClassEntity],
  findAlternativesWithDistance: (TypeEntity) => Seq[(TypeEntity, Int)]) {

  /**
   * Transforms a parsed query into a query that can be passed to the terms index.
   *
   * Fails when `findClassesBySuffix` or `findSubClasses` fails.
   */
  def apply(raw: RawQuery): SemanticError \/ ApiQuery =
    resolveNames(raw.tpe).map(
      (toType _) andThen
        (_.normalize(Nil)) andThen
        (tpe => QueryFingerprint(
          findAlternativesWithDistance,
          tpe)) andThen
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

  /**
   * Builds the query structure of parts and alternatives for a type.
   */
  def expandQuery(tpe: TypeEntity): ExpandedQuery = {
    import ExpandedQuery._

    def parts(tpe: TypeEntity, depth: Int, dist: Int): ExpandedQuery = {
      Sum(Leaf(tpe.withArgsAsParams, depth, dist) ::
        tpe.args.filterNot(_.isTypeParam).map(arg => alternatives(arg, depth + 1)))
    }

    def alternatives(tpe: TypeEntity, depth: Int): ExpandedQuery = {
      val originalTypeParts = parts(tpe, depth, 0)
      val alternativesParts =
        findAlternativesWithDistance(tpe).toList.map {
          case (alt, dist) =>
            parts(alt, depth, dist)
        }

      Max(originalTypeParts :: alternativesParts)
    }

    Sum(for {
      arg <- tpe.args
    } yield alternatives(arg, 0))
  }

  private def toApiQuery(fingerprint: QueryFingerprint): ApiQuery = {
    val flattenedTypes = fingerprint.types.zipWithIndex.flatMap {
      case (tpe, idx) => tpe.alternatives.map(alt =>
        (idx, tpe.variance, alt.typeName, boost(tpe, alt)))
    }

    val typesByVarianceAndName = flattenedTypes.groupBy(t => (t._2 /* variance */ , t._3 /* name */ )).values

    val withOccurrenceIndex = typesByVarianceAndName.map { types =>
      types.sortBy(-_._4 /* boost */ ).zipWithIndex.map {
        case ((tpeIndex, variance, altName, boost), occIndex) =>
          (tpeIndex, ApiQuery.Alternative(variance, altName, occIndex, boost))
      }
    }.flatten

    val queryTypes = withOccurrenceIndex.groupBy(t => (t._1 /* original type index */ )).map {
      case (tpeIndex, alts) => ApiQuery.Type(alts.map(_._2).toList)
    }

    ApiQuery(Nil, queryTypes.toList)
  }

  private def boost(tpe: QueryFingerprint.Type, alt: QueryFingerprint.Alternative): Double = {
    val maxFrequency = settings.index.typeFrequenciesSampleSize

    val freq = math.min(getFrequency(tpe.variance, alt.typeName), maxFrequency)
    val itf = math.log((maxFrequency.toDouble + 1) / (freq + 1))

    weightedGeometricMean(
      itf -> settings.query.typeFrequencyWeight,
      1d / (tpe.depth + 1) -> settings.query.depthBoostWeight,
      1d / (alt.distance + 1) -> settings.query.distanceBoostWeight)
  }

  /**
   * Implements http://en.wikipedia.org/wiki/Weighted_geometric_mean
   */
  private def weightedGeometricMean(elemsWithWeight: (Double, Double)*) =
    math.exp(
      elemsWithWeight.map { case (x, w) => w * math.log(x) }.sum / elemsWithWeight.map { case (_, w) => w }.sum)

  private def weightedArithmeticMean(elemsWithWeight: (Double, Double)*) =
    elemsWithWeight.map { case (x, w) => x * w }.sum / elemsWithWeight.map { case (_, w) => w }.sum

  private def getFrequency(v: Variance, t: String) =
    findClassesBySuffix(t).headOption.map(_.frequency(v)).filter(_ > 0).getOrElse(1)
}

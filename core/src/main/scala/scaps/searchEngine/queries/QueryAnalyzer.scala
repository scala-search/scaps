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
import scaps.searchEngine.index.TypeFrequencies
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
import scaps.webapi.TermEntity

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
        (tpe => Fingerprint.queryFingerprint(
          (name: String) => findClassesBySuffix(name).headOption,
          findSubClasses,
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

  private def toApiQuery(fingerprint: Fingerprint): APIQuery = {
    val tpes = fingerprint
      .typesWithOccurrenceIndex(Ordering[Double].on(fpt => -boost(fpt)))
      .map {
        case (tpe, idx) =>
          APIQuery.Type(tpe.variance, tpe.name, idx, boost(tpe))
      }

    APIQuery(Nil, tpes.toList.sortBy(-_.boost))
  }

  private def boost(tpe: Fingerprint.Type): Double = {
    val maxFrequency = TypeFrequencies.termsSampleSize

    val freq = math.min(getFrequency(tpe.variance, tpe.name), maxFrequency)
    val itf = math.log((maxFrequency.toDouble + 1) / (freq + 1))

    weightedGeometricMean(
      itf -> settings.typeFrequencyWeight,
      1d / (tpe.depth + 1) -> settings.depthBoostWeight,
      1d / (tpe.distance + 1) -> settings.distanceBoostWeight)
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

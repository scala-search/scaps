package scaps.searchEngine.queries

import scalaz.{ \/ => \/ }
import scalaz.std.list.listInstance
import scalaz.syntax.traverse.ToTraverseOps
import scaps.searchEngine.ApiQuery
import scaps.searchEngine.ApiTypeQuery
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
  import ExpandedQuery._

  def children: List[ExpandedQuery]
}

private[queries] object ExpandedQuery {
  sealed trait Part extends ExpandedQuery
  sealed trait Alternative extends ExpandedQuery

  case class Sum(parts: List[Part]) extends Alternative {
    val children = parts

    override def toString =
      parts.mkString("sum(", ", ", ")")
  }
  object Sum {
    def apply(parts: Part*): Sum =
      Sum(parts.toList)
  }

  case class Max(alternatives: List[Alternative]) extends Part {
    val children = alternatives

    override def toString =
      alternatives.mkString("max(", ", ", ")")
  }
  object Max {
    def apply(alts: Alternative*): Max =
      Max(alts.toList)
  }

  case class Leaf(tpe: TypeEntity, fraction: Double, depth: Int, dist: Int) extends Part with Alternative {
    val children = Nil

    override def toString =
      s"$tpe^($fraction, $depth, $dist)"
  }

  def minimizeClauses(q: ExpandedQuery): ExpandedQuery = q match {
    case l: Leaf           => l
    case Sum(parts)        => q
    case Max(alternatives) => q
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
        (expandQuery _) andThen
        (toApiTypeQuery _) andThen
        { typeQuery => ApiQuery(raw.keywords, typeQuery) })

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
  def expandQuery(tpe: TypeEntity): ExpandedQuery.Alternative = {
    import ExpandedQuery._

    def parts(tpe: TypeEntity, fraction: Double, depth: Int, dist: Int, outerTpes: Set[TypeEntity]): Alternative = {
      tpe match {
        case TypeEntity.Ignored(args, v) =>
          val partFraction = if (tpe.args.isEmpty) fraction else (fraction / tpe.args.length)

          Sum(args.map(alternatives(_, partFraction, depth, outerTpes)))
        case tpe =>
          val partArgs = tpe.args
            .filterNot(_.isTypeParam)

          val outerFraction = if (partArgs.isEmpty) fraction else (fraction / 2)
          val partFraction = (fraction / (2 * partArgs.length))

          val parts = partArgs.map(arg =>
            if (outerTpes.contains(arg)) Leaf(arg.withArgsAsParams, partFraction, depth + 1, 0)
            else alternatives(arg, partFraction, depth + 1, outerTpes))

          Sum(Leaf(tpe.withArgsAsParams, outerFraction, depth, dist) :: parts)
      }
    }

    def alternatives(tpe: TypeEntity, fraction: Double, depth: Int, outerTpes: Set[TypeEntity]): Part = {
      val alternativesWithDistance = findAlternativesWithDistance(tpe).toList

      val outerTpesAndAlts = outerTpes + tpe ++ alternativesWithDistance.map(_._1)

      val originalTypeParts = parts(tpe, fraction, depth, 0, outerTpesAndAlts)
      val alternativesParts =
        alternativesWithDistance.map {
          case (alt, dist) =>
            parts(alt, fraction, depth, dist, outerTpesAndAlts)
        }

      Max(originalTypeParts :: alternativesParts)
    }

    tpe match {
      case TypeEntity.Ignored(_, _) =>
        parts(tpe, 1, 0, 0, Set())
      case _ =>
        parts(TypeEntity.Ignored(tpe :: Nil, Covariant), 1, 0, 0, Set())
    }
  }

  private def toApiTypeQuery(q: ExpandedQuery): ApiTypeQuery = q match {
    case ExpandedQuery.Sum(parts) => ApiTypeQuery.Sum(parts.map(toApiTypeQuery))
    case ExpandedQuery.Max(alts)  => ApiTypeQuery.Max(alts.map(toApiTypeQuery))
    case l: ExpandedQuery.Leaf    => ApiTypeQuery.Type(l.tpe.variance, l.tpe.name, boost(l))
  }

  private def boost(l: ExpandedQuery.Leaf): Double =
    l.fraction * weightedGeometricMean(
      itf(l) -> settings.query.typeFrequencyWeight,
      1d / (l.depth + 1) -> settings.query.depthBoostWeight,
      1d / (l.dist + 1) -> settings.query.distanceBoostWeight)

  /**
   * The inverse type frequency is defined as log10(10 / (10f + (1 - 2f)))
   * where f is the type frequency normed by the maximum possible type frequency.
   *
   * The (1 - 2f) term ensures that the return value is <= 1.0 but > 0.0 for types
   * with a frequency equal to the maximum frequency.
   */
  private def itf(l: ExpandedQuery.Leaf): Double = {
    val maxFrequency = settings.index.typeFrequenciesSampleSize

    val freq = math.min(getFrequency(l.tpe.variance, l.tpe.name), maxFrequency)
    val normedFreq = (freq.toDouble / maxFrequency)
    math.log10(10 / (normedFreq * 10 + (1 - 2 * normedFreq)))
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

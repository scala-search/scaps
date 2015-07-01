package scaps.searchEngine.queries

import scalaz.{ \/ => \/ }
import scalaz.std.list.listInstance
import scalaz.syntax.traverse.ToTraverseOps
import scaps.searchEngine.ApiQuery
import scaps.searchEngine.ApiTypeQuery
import scaps.searchEngine.NameAmbiguous
import scaps.searchEngine.NameNotFound
import scaps.searchEngine.SemanticError
import scaps.searchEngine.UnexpectedNumberOfTypeArgs
import scaps.settings.QuerySettings
import scaps.webapi.ClassEntity
import scaps.webapi.Contravariant
import scaps.webapi.Covariant
import scaps.webapi.Invariant
import scaps.webapi.TypeEntity
import scaps.webapi.Variance
import scaps.searchEngine.View
import scaps.utils._

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

  def minimize(p: Part): Part = p match {
    case Max((alt: Leaf) :: Nil) => alt
    case Max(alts) =>
      val minAlts = alts.map(minimize)

      maxRepeatedPart(minAlts).fold[Part] {
        Max(minAlts)
      } { part =>
        minimize(Max(factorOut(part, minAlts)))
      }
    case _ => p
  }

  private def maxRepeatedPart(alts: List[Alternative]): Option[Part] =
    alts
      .flatMap {
        case Sum(parts) => parts.distinct
        case _          => Nil
      }
      .groupBy(identity)
      .mapValues(_.length)
      .filter(_._2 > 1)
      .maxByOpt(_._2)
      .map(_._1)

  private def factorOut(part: Part, alts: List[Alternative]): List[Alternative] = {
    val (altsWithPart, altsWithoutPart) = alts.partition {
      case Sum(ps) => ps.contains(part)
      case _       => false
    }

    val altsMinusPart = altsWithPart.map {
      case Sum(ps) => Sum(ps.filter(_ != part))
      case _       => ???
    }

    Sum(Max(altsMinusPart) :: part :: Nil) :: altsWithoutPart
  }

  def minimize(a: Alternative): Alternative = a match {
    case Sum((part: Leaf) :: Nil) => part
    case Sum(parts)               => Sum(parts.map(minimize))
    case _                        => a
  }
}

/**
 * Analyzes parsed queries.
 *
 * Instances require access to the class and view index which can be injected via
 * `findClassesBySuffix` and `findAlternativesWithDistance`
 */
class QueryAnalyzer private[searchEngine] (
  settings: QuerySettings,
  findClassesBySuffix: (String) => Seq[ClassEntity],
  findAlternativesWithDistance: (TypeEntity) => Seq[(TypeEntity, Int)]) {

  /**
   * Transforms a parsed query into a query that can be passed to the terms index.
   */
  def apply(raw: RawQuery): SemanticError \/ ApiQuery =
    resolveNames(raw.tpe).map(
      (toType _) andThen
        (_.normalize(Nil)) andThen
        (expandQuery _) andThen
        (ExpandedQuery.minimize(_)) andThen
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
  private[queries] def expandQuery(tpe: TypeEntity): ExpandedQuery.Alternative = {
    import ExpandedQuery._

    def parts(tpe: TypeEntity, fraction: Double, depth: Int, dist: Int, outerTpes: Set[TypeEntity]): Alternative = {
      tpe match {
        case TypeEntity.Ignored(args, v) =>
          val partFraction = if (tpe.args.isEmpty) fraction else (fraction / tpe.args.length)

          Sum(args.map(alternatives(_, partFraction, depth, outerTpes)))
        case tpe =>
          val partArgs = tpe.args
            .filterNot(_.isTypeParam)

          val partFraction = (fraction / (partArgs.length + 1))

          val parts = partArgs.map(arg =>
            if (outerTpes.contains(arg)) Leaf(arg.withArgsAsParams, partFraction, depth + 1, 0)
            else alternatives(arg, partFraction, depth + 1, outerTpes))

          Sum(Leaf(tpe.withArgsAsParams, partFraction, depth, dist) :: parts)
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

    val noParts = tpe.toList
      .count {
        case TypeEntity.Ignored(_, _) => false
        case _                        => true
      }

    tpe match {
      case TypeEntity.Ignored(args, _) =>
        parts(tpe, noParts, 0, 0, Set())
      case _ =>
        parts(TypeEntity.Ignored(tpe :: Nil, Covariant), noParts, 0, 0, Set())
    }
  }

  private def toApiTypeQuery(q: ExpandedQuery): ApiTypeQuery = q match {
    case ExpandedQuery.Sum(parts) =>
      ApiTypeQuery.Sum(parts.map(toApiTypeQuery))
    case ExpandedQuery.Max(alts) =>
      ApiTypeQuery.Max(alts.map(toApiTypeQuery))
    case l: ExpandedQuery.Leaf =>
      ApiTypeQuery.Type(
        l.tpe.variance,
        l.tpe.name,
        boost(l),
        getFrequency(l.tpe.variance, l.tpe.name))
  }

  private def boost(l: ExpandedQuery.Leaf): Double =
    weightedHarmonicMean(
      l.fraction -> settings.fractionWeight,
      itf(l) -> settings.typeFrequencyWeight,
      1d / (l.depth + 1) -> settings.depthBoostWeight,
      1d / (l.dist + 1) -> settings.distanceBoostWeight)

  /**
   * The inverse type frequency is defined as log10(10 / (10f + (1 - f)))
   * where f is the type frequency normed by the maximum possible type frequency
   * (see TypeFrequencies).
   */
  private def itf(l: ExpandedQuery.Leaf): Double = {
    val freq = getFrequency(l.tpe.variance, l.tpe.name)
    math.log10(10 / (freq * 10 + (1 - freq)))
  }

  /**
   * Implements http://en.wikipedia.org/wiki/Weighted_geometric_mean
   */
  private def weightedGeometricMean(elemsWithWeight: (Double, Double)*) =
    math.exp(
      elemsWithWeight.map { case (x, w) => w * math.log(x) }.sum / elemsWithWeight.map { case (_, w) => w }.sum)

  private def weightedArithmeticMean(elemsWithWeight: (Double, Double)*) =
    elemsWithWeight.map { case (x, w) => x * w }.sum / elemsWithWeight.map { case (_, w) => w }.sum

  /**
   * Implements https://en.wikipedia.org/wiki/Harmonic_mean#Weighted_harmonic_mean
   */
  private def weightedHarmonicMean(elemsWithWeight: (Double, Double)*) =
    elemsWithWeight.map(_._2).sum / elemsWithWeight.map { case (x, w) => w / x }.sum

  private def getFrequency(v: Variance, t: String) =
    findClassesBySuffix(t).headOption.map(_.frequency(v)).getOrElse(0f)
}

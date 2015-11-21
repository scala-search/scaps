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
import scaps.api.TypeDef
import scaps.api.Contravariant
import scaps.api.Covariant
import scaps.api.Invariant
import scaps.api.TypeRef
import scaps.api.Variance
import scaps.utils._
import scaps.searchEngine.MaximumClauseCountExceededException
import scaps.api.FingerprintTerm

private[queries] sealed trait ResolvedQuery
private[queries] object ResolvedQuery {
  case object Wildcard extends ResolvedQuery
  case class Type(cls: TypeDef, args: List[ResolvedQuery]) extends ResolvedQuery
}

private[queries] sealed trait ExpandedQuery {
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

  case class Leaf(tpe: TypeRef, fraction: Double, depth: Int, dist: Float) extends Part with Alternative {
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
 * `findTypeDefsBySuffix` and `findAlternativesWithDistance`
 */
class QueryAnalyzer private[searchEngine] (
    polarizedTypes: Boolean,
    settings: QuerySettings,
    findTypeDefsBySuffix: (String) => Seq[TypeDef],
    findAlternativesWithDistance: (TypeRef) => Seq[(TypeRef, Float)]) {

  /**
   * Transforms a parsed query into a query that can be passed to the values index.
   */
  def apply(raw: RawQuery): SemanticError \/ ApiQuery =
    raw match {
      case RawQuery.Keywords(keys) =>
        \/.right(ApiQuery(keys, None))
      case RawQuery.Full(keys, tpe) =>
        for {
          resolved <- resolveNames(tpe)
          normalized = toType(resolved).normalize(Nil)
          polarized = if (polarizedTypes) normalized else normalized.withVariance(Invariant)
          expanded = ExpandedQuery.minimize(expandQuery(polarized))
          typeQuery = toApiTypeQuery(expanded)
        } yield {
          ApiQuery(keys, Some(typeQuery))
        }
    }

  /**
   * Resolves all type names in the query and assigns the according class entities.
   *
   * Names that cannot be resolved and have length 1 are treated as type parameters.
   */
  private def resolveNames(raw: RawQuery.Type): SemanticError \/ ResolvedQuery = {
    val resolvedArgs: SemanticError \/ List[ResolvedQuery] =
      raw.args.map(arg => resolveNames(arg)).sequenceU

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

  private def isTypeParam(name: String): Boolean =
    name.length() == 1 && (name(0).isLetter || name(0) == '_')

  private def toType(resolved: ResolvedQuery): TypeRef = {
    def rec(resolved: ResolvedQuery, variance: Variance): TypeRef =
      resolved match {
        case ResolvedQuery.Wildcard =>
          TypeRef.Unknown(variance)
        case ResolvedQuery.Type(cls, args) =>
          val tpeArgs = cls.typeParameters.zip(args).map {
            case (tpeParam, ResolvedQuery.Wildcard) =>
              (variance * tpeParam.variance) match {
                case Covariant     => TypeRef(tpeParam.lowerBound, Covariant, Nil)
                case Contravariant => TypeRef(tpeParam.upperBound, Contravariant, Nil)
                case Invariant     => TypeRef.Unknown(Invariant)
              }
            case (tpeParam, arg) =>
              rec(arg, variance * tpeParam.variance)
          }
          TypeRef(cls.name, variance, tpeArgs)
      }

    rec(resolved, Covariant)
  }

  /**
   * Builds the query structure of parts and alternatives for a type.
   */
  private[queries] def expandQuery(tpe: TypeRef): ExpandedQuery.Alternative = {
    import ExpandedQuery._

    var clauseCount = 0

    def increaseCount() = {
      clauseCount += 1
      if (clauseCount > settings.maxClauseCount)
        throw MaximumClauseCountExceededException
    }

    def parts(tpe: TypeRef, depth: Int, dist: Float, outerTpes: Set[TypeRef], termSpecifity: TermSpecifity): Alternative = {
      increaseCount()

      tpe match {
        case TypeRef.Ignored(args, v) =>
          Sum(args.map { arg =>
            alternatives(arg, depth, outerTpes, termSpecifity.crop(arg))
          })
        case tpe =>
          val partArgs = tpe.args
            .filterNot(_.isTypeParam)

          val parts = partArgs.map { arg =>
            if (outerTpes.contains(arg)) Leaf(arg.withArgsAsParams, termSpecifity(arg.term), depth + 1, 1)
            else alternatives(arg, depth + 1, outerTpes, termSpecifity.crop(arg))
          }

          Sum(Leaf(tpe.withArgsAsParams, termSpecifity(tpe.term), depth, dist) :: parts)
      }
    }

    def alternatives(tpe: TypeRef, depth: Int, outerTpes: Set[TypeRef], termSpecifity: TermSpecifity): Part = {
      increaseCount()

      val alternativesWithDistance =
        if (settings.views) findAlternativesWithDistance(tpe).toList
        else Nil

      val outerTpesAndAlts = outerTpes + tpe ++ alternativesWithDistance.map(_._1)

      val originalTypeParts = parts(tpe, depth, 1, outerTpesAndAlts, termSpecifity)
      val alternativesParts =
        alternativesWithDistance.map {
          case (alt, dist) =>
            parts(alt, depth, dist, outerTpesAndAlts, termSpecifity.updateForAlternative(tpe, alt))
        }

      Max(originalTypeParts :: alternativesParts)
    }

    tpe match {
      case TypeRef.Ignored(_, _) =>
        parts(tpe, 0, 0, Set(), TermSpecifity(tpe, 1))
      case _ =>
        val itpe = TypeRef.Ignored(tpe :: Nil, Covariant)
        parts(itpe, 0, 0, Set(), TermSpecifity(itpe, 1))
    }
  }

  case class TermSpecifity(specifities: Map[FingerprintTerm, Double]) {
    def apply(term: FingerprintTerm) = specifities(term)

    def crop(part: TypeRef) = {
      val fp = part.fingerprint
      TermSpecifity(specifities.filterKeys { t => fp.contains(t) })
    }

    def updateForAlternative(orig: TypeRef, alt: TypeRef) = {
      val outerTerm = orig.term
      val innerTerms = orig.args.flatMap(_.fingerprint)
      val newOuter = alt.term
      val newInner = alt.args.flatMap(_.fingerprint)

      if (newInner.length < innerTerms.length || newInner.exists { t => !innerTerms.contains(t) }) {
        TermSpecifity(alt, specifities.values.sum)
      } else {
        TermSpecifity(specifities + (newOuter -> specifities(outerTerm))).crop(alt)
      }
    }
  }

  object TermSpecifity {
    def apply(tpe: TypeRef, norm: Double): TermSpecifity = {
      val fp = tpe.fingerprint

      val itfs = tpe.fingerprint.map(t => (t, math.sqrt(1 - getFrequency(t))))
      val sum = itfs.map(_._2).sum

      TermSpecifity(itfs.map { case (t, itf) => (t, norm * itf / sum) }.toMap)
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
        getFrequency(l.tpe.term))
  }

  private val boost: (ExpandedQuery.Leaf => Double) = { l =>
    l.fraction * Statistic.weightedGeometricMean(
      itf(l.tpe.term) -> settings.typeFrequencyWeight,
      1d / (l.depth + 1) -> settings.depthBoostWeight,
      l.dist.toDouble -> settings.distanceBoostWeight)
  }

  /**
   * The inverse type frequency is defined as log10(10 / (10f + (1 - f)))
   * where f is the type frequency normed by the maximum possible type frequency
   * (see TypeFrequencies).
   */
  private def itf(t: FingerprintTerm): Double = {
    val freq = getFrequency(t)
    math.log10(10 / (freq * 10 + (1 - freq)))
  }

  private def getFrequency(t: FingerprintTerm) =
    findTypeDefsBySuffix(t.tpe).headOption.map(_.frequency(t.variance)).getOrElse(0f)
}

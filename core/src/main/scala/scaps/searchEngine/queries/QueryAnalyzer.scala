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
import scaps.api.ViewDef
import scaps.api.ValueDef

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
      case Sum(ps) => Sum(ps diff List(part))
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
 * `findTypeDefsBySuffix` and `findViews`
 */
class QueryAnalyzer private[searchEngine] (
    polarizedTypes: Boolean,
    settings: QuerySettings,
    findTypeDefsBySuffix: (String) => Seq[TypeDef],
    findViews: (TypeRef) => Seq[ViewDef]) {

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
          typeQuery = apply(normalized)
        } yield {
          ApiQuery(keys, Some(typeQuery))
        }
    }

  def apply(v: ValueDef): ApiTypeQuery =
    apply(v.tpe.normalize(v.typeParameters))

  def apply(t: TypeRef): ApiTypeQuery = {
    val polarized = if (polarizedTypes) t else t.withVariance(Invariant)
    val expanded = ExpandedQuery.minimize(expandQuery(t))
    toApiTypeQuery(expanded)
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

    def parts(tpe: TypeRef, depth: Int, dist: Float, outerTpes: Set[TypeRef], fraction: Double): Alternative = {
      increaseCount()

      tpe match {
        case TypeRef.Ignored(args, v) =>
          Sum(args.map { arg =>
            val partF = fraction / args.length
            alternatives(arg, depth, outerTpes, partF)
          })
        case tpe =>
          val partArgs = tpe.args
            .filterNot(_.isTypeParam)

          val partF = fraction / (1 + partArgs.length)

          val parts = partArgs.map { arg =>
            if (outerTpes.contains(arg)) Leaf(arg.withArgsAsParams, partF, depth + 1, 1)
            else alternatives(arg, depth + 1, outerTpes, partF)
          }

          Sum(Leaf(tpe.withArgsAsParams, partF, depth, dist) :: parts)
      }
    }

    def alternatives(tpe: TypeRef, depth: Int, outerTpes: Set[TypeRef], fraction: Double): Part = {
      increaseCount()

      val alternativesWithDistance =
        (if (settings.views) findViews(tpe).toList else Nil)
          .flatMap(v => v(tpe).map((_, v.distance, v.retainedInformation)))
          .distinct

      val outerTpesAndAlts = outerTpes + tpe ++ alternativesWithDistance.map(_._1)

      val originalTypeParts = parts(tpe, depth, 1, outerTpesAndAlts, fraction)
      val alternativesParts =
        alternativesWithDistance.map {
          case (alt, dist, retainedInfo) =>
            parts(alt, depth, dist, outerTpesAndAlts, fraction * retainedInfo)
        }

      Max(originalTypeParts :: alternativesParts)
    }

    tpe match {
      case TypeRef.Ignored(_, _) =>
        parts(tpe, 0, 0, Set(), 1)
      case _ =>
        val itpe = TypeRef.Ignored(tpe :: Nil, Covariant)
        parts(itpe, 0, 0, Set(), 1)
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
    (if (settings.fractions) l.fraction else 1d) *
      itf(l.tpe.term) *
      math.pow(l.dist, settings.distanceBoostWeight)
  }

  /**
   * The inverse type frequency is defined as log10(10 / (10f + (1 - f)))
   * where f is the type frequency normed by the maximum possible type frequency
   * (see TypeFrequencies).
   */
  private def itf(t: FingerprintTerm): Double = {
    val base = settings.typeFrequencyWeight
    if (base == 0) {
      1
    } else {
      val freq = getFrequency(t)
      math.log(base / (freq * base + (1 - freq))) / math.log(base)
    }
  }

  private def getFrequency(t: FingerprintTerm) =
    findTypeDefsBySuffix(t.tpe).headOption.map(_.frequency(t.variance)).getOrElse(0f)
}

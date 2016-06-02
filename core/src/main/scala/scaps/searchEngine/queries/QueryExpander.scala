/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine.queries

import scaps.api.Covariant
import scaps.api.FingerprintTerm
import scaps.api.TypeDef
import scaps.api.TypeRef
import scaps.api.ViewDef
import scaps.searchEngine.ApiTypeQuery
import scaps.searchEngine.MaximumClauseCountExceeded
import scaps.settings.QuerySettings
import scaps.utils.TraversableOps
import scala.util.Try
import scalaz.\/
import scaps.searchEngine.SemanticError

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

  case class Leaf(tpe: TypeRef, fraction: Double, distance: Int) extends Part with Alternative {
    val children = Nil

    override def toString =
      s"$tpe^($fraction,$distance)"
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
 * Creates APIQueries from (normalized) type refs.
 */
class QueryExpander(
    settings: QuerySettings,
    getTypeFrequency: FingerprintTerm => Double,
    findViews: (TypeRef) => Seq[(TypeRef, Double)]) {

  private case object MaximumClauseCountExceededException extends Exception

  def apply(tpe: TypeRef): SemanticError \/ ApiTypeQuery = {
    try {
      val expanded = expandQuery(tpe)
      val compacted = ExpandedQuery.minimize(expanded)
      \/.right(toApiTypeQuery(compacted))
    } catch {
      case MaximumClauseCountExceededException =>
        \/.left(MaximumClauseCountExceeded)
    }
  }

  private[queries] def expandQuery(tpe: TypeRef): ExpandedQuery.Alternative = {
    import ExpandedQuery._

    var clauseCount = 0

    def increaseCount() = {
      clauseCount += 1
      if (clauseCount > settings.maxClauseCount)
        throw MaximumClauseCountExceededException
    }

    def parts(tpe: TypeRef, outerTpes: Set[TypeRef], fraction: Double, distance: Int): Alternative = {
      increaseCount()

      tpe match {
        case TypeRef.Ignored(args, v) =>
          Sum(args.map { arg =>
            val partF = fraction / args.length
            alternatives(arg, outerTpes, partF)
          })
        case tpe =>
          val partArgs = tpe.args
            .filterNot(_.isTypeParam)

          val partF = fraction / (1 + partArgs.length)

          val parts = partArgs.map { arg =>
            if (outerTpes.contains(arg)) Leaf(arg.withArgsAsParams, partF, 0)
            else alternatives(arg, outerTpes, partF)
          }

          Sum(Leaf(tpe.withArgsAsParams, partF, distance) :: parts)
      }
    }

    def alternatives(tpe: TypeRef, outerTpes: Set[TypeRef], fraction: Double): Part = {
      increaseCount()

      val alternativesWithRetainedInfo =
        (if (settings.views) findViews(tpe).toList else Nil)
          .distinct

      val outerTpesAndAlts = outerTpes + tpe ++ alternativesWithRetainedInfo.map(_._1)

      val originalTypeParts = parts(tpe, outerTpesAndAlts, fraction, 0)
      val alternativesParts =
        alternativesWithRetainedInfo.map {
          case (alt, retainedInfo) =>
            parts(alt, outerTpesAndAlts, fraction * retainedInfo, 1)
        }

      Max(originalTypeParts :: alternativesParts)
    }

    tpe match {
      case TypeRef.Ignored(_, _) =>
        parts(tpe, Set(), 1, 0)
      case _ =>
        val itpe = TypeRef.Ignored(tpe :: Nil, Covariant)
        parts(itpe, Set(), 1, 0)
    }
  }

  private val boost: (ExpandedQuery.Leaf => Double) = { l =>
    (if (settings.fractions) l.fraction else 1d) * (-settings.distanceWeight * l.distance + 1) * itf(l.tpe.term)
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
      val freq = getTypeFrequency(t)
      math.max(math.log(base / (freq * base + (1 - freq))) / math.log(base), 0.001)
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
        getTypeFrequency(l.tpe.term))
  }
}

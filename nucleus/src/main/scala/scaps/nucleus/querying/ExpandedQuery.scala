/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.querying

import scaps.nucleus.TypeRef
import scaps.nucleus.Variance

private[nucleus] sealed trait ExpandedQuery {
  import ExpandedQuery._

  def children: List[ExpandedQuery]

  def pretty(indent: String = ""): String =
    this match {
      case Sum(parts) => parts.map(indent + " " + _.pretty(indent + " ")).mkString(s"${indent}sum(\n", ",\n", ")")
      case Max(alts)  => alts.map(indent + " " + _.pretty(indent + " ")).mkString(s"${indent}max(\n", ",\n", ")")
      case l: Leaf    => indent + l.toString()
    }

  def minimize(): ExpandedQuery = {
    val min = this match {
      case a: Alternative => ExpandedQuery.minimize(a)
      case p: Part        => ExpandedQuery.minimize(p)
    }

    if (min == this)
      min
    else
      min.minimize()
  }
}

private[nucleus] object ExpandedQuery {
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

  case class Leaf(variance: Variance, name: String, fraction: Double, depth: Int, dist: Float) extends Part with Alternative {
    val children = Nil

    override def toString =
      s"${variance.prefix}$name^($fraction, $depth, $dist)"
  }

  def minimize(p: Part): Part =
    p match {
      case Max((alt: Leaf) :: Nil)   => alt
      case Max(Sum(p :: Nil) :: Nil) => p
      case Max(alts) =>
        val minAlts = alts.map(minimize)

        maxRepeatedPart(minAlts).fold[Part] {
          Max(minAlts)
        } { part =>
          minimize(Max(factorOut(part, minAlts)))
        }
      case _ => p
    }

  private def maxRepeatedPart(alts: List[Alternative]): Option[Part] = {
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
  }

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
    case Sum((part: Leaf) :: Nil)  => part
    case Sum(Max(a :: Nil) :: Nil) => a
    case Sum(parts)                => Sum(parts.map(minimize))
    case _                         => a
  }
}

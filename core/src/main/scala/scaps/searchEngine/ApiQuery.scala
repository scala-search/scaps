/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine

import scaps.api.Variance

case class ApiQuery(keywords: String, tpe: Option[ApiTypeQuery]) {
  override def toString =
    s""""$keywords": $tpe"""

  def allTypes = tpe.toSeq.flatMap(_.allTypes)

  def prettyPrint = tpe.fold { keywords } { t => s""""$keywords": ${t.prettyPrint()}""" }
}

sealed trait ApiTypeQuery {
  import ApiTypeQuery._

  def children: List[ApiTypeQuery]

  def allTypes: List[Type] = this match {
    case t: Type => List(t)
    case _       => children.flatMap(_.allTypes)
  }

  def prettyPrint(level: Int = 0): String = {
    val prefix = " " * (level * 2)

    this match {
      case Sum(Nil) | Sum(_ :: Nil) | Max(Nil) | Max(_ :: Nil) | Type(_, _, _, _) =>
        s"$prefix$this"
      case Sum(cs) =>
        cs.map(_.prettyPrint(level + 1)).mkString(s"${prefix}sum(\n", "\n", ")")
      case Max(cs) =>
        cs.map(_.prettyPrint(level + 1)).mkString(s"${prefix}max(\n", "\n", ")")
    }
  }
}

object ApiTypeQuery {
  case class Sum(parts: List[ApiTypeQuery]) extends ApiTypeQuery {
    def children = parts

    override def toString =
      parts.mkString("sum(", ", ", ")")
  }

  object Sum {
    def apply(parts: ApiTypeQuery*): Sum = Sum(parts.toList)
  }

  case class Max(alternatives: List[ApiTypeQuery]) extends ApiTypeQuery {
    def children = alternatives

    override def toString =
      alternatives.mkString("max(", ", ", ")")
  }

  object Max {
    def apply(alternatives: ApiTypeQuery*): Max = Max(alternatives.toList)
  }

  case class Type(variance: Variance, typeName: String, boost: Double, typeFrequency: Double) extends ApiTypeQuery {
    def children = Nil

    val fingerprint = s"${variance.prefix}${typeName}"

    override def toString =
      s"${variance.prefix}${typeName}^($boost, $typeFrequency)"
  }
}

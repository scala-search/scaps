/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.querying

import scaps.nucleus.Type
import scaps.nucleus.TypeRef
import scaps.nucleus.TypeParam
import scaps.nucleus.Covariant
import scaps.nucleus.indexing.TypeView
import scaps.nucleus.indexing.TypeViewIndex
import scaps.nucleus.IndexAccess
import scaps.nucleus.indexing.TypeNormalization

object QueryExpansion {

  import scaps.nucleus.indexing.{ InternalTypes => I }

  private[querying] def expandQuery(tpe: Type, viewsFrom: Type => Seq[TypeView]): ExpandedQuery = {
    import ExpandedQuery._

    def parts(tr: TypeRef, depth: Int, dist: Float, outerTpes: Set[TypeRef], fraction: Double): Alternative = {
      tr match {
        case I.Ignored(v, args) =>
          Sum(args.map { arg =>
            val partF = fraction / args.length
            alternatives(arg, depth, outerTpes, partF)
          })
        case _ =>
          val partF = fraction / (1 + tr.args.length)

          val parts = tr.args.map { arg =>
            if (outerTpes.contains(arg)) Leaf(arg.variance, arg.name, partF, depth + 1, 1)
            else alternatives(arg, depth + 1, outerTpes, partF)
          }

          Sum(Leaf(tr.variance, tr.name, partF, depth, dist) :: parts)
      }
    }

    def alternatives(tr: TypeRef, depth: Int, outerTpes: Set[TypeRef], fraction: Double): Part = {
      val alternativesWithDistance =
        viewsFrom(Type(tpe.params, tr))
          .flatMap { v =>
            v(tr).map { alt =>
              (TypeNormalization.substituteTypeParams(Type(tpe.params, alt)), v.distance, v.retainedInformation)
            }
          }
          .distinct
          .toList

      val outerTpesAndAlts = outerTpes ++ alternativesWithDistance.map(_._1)

      val alternativesParts =
        alternativesWithDistance.map {
          case (alt, dist, retainedInfo) =>
            parts(alt, depth, dist, outerTpesAndAlts, fraction * retainedInfo)
        }

      Max(alternativesParts)
    }

    val expanded = ((TypeNormalization.substituteTypeParams _) andThen (TypeNormalization.normalize _))(tpe) match {
      case f @ I.Fn(v, args, res) =>
        val itpe = I.Ignored(v, args :+ res)
        parts(itpe, 0, 0, Set(), 1)
      case tr @ I.Ignored(_, _) =>
        parts(tr, 0, 0, Set(), 1)
      case tr =>
        val itpe = I.Ignored(Covariant, tr :: Nil)
        parts(itpe, 0, 0, Set(), 1)
    }

    expanded.minimize()
  }
}

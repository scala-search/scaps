package scaps.nucleus.querying

import scaps.nucleus.Type
import scaps.nucleus.TypeRef
import scaps.nucleus.TypeParam
import scaps.nucleus.Covariant
import scaps.nucleus.indexing.TypeView

class QueryExpander(findViews: TypeRef => List[TypeView]) {

  import scaps.nucleus.indexing.{ InternalTypes => I }

  def expandQuery(query: Type): ExpandedQuery = {
    ???
  }

  private[querying] def expandQuery(typeParams: List[TypeParam], tpe: TypeRef): ExpandedQuery.Alternative = {
    import ExpandedQuery._

    def isTypeParam(t: TypeRef) = typeParams.exists(_.name == t.name)

    def parts(tpe: TypeRef, depth: Int, dist: Float, outerTpes: Set[TypeRef], fraction: Double): Alternative = {
      tpe match {
        case I.Ignored(v, args) =>
          Sum(args.map { arg =>
            val partF = fraction / args.length
            alternatives(arg, depth, outerTpes, partF)
          })
        case tpe =>
          val partArgs = tpe.args
            .filterNot(isTypeParam)

          val partF = fraction / (1 + partArgs.length)

          val parts = partArgs.map { arg =>
            if (outerTpes.contains(arg)) Leaf(arg, partF, depth + 1, 1)
            else alternatives(arg, depth + 1, outerTpes, partF)
          }

          Sum(Leaf(tpe, partF, depth, dist) :: parts)
      }
    }

    def alternatives(tpe: TypeRef, depth: Int, outerTpes: Set[TypeRef], fraction: Double): Part = {

      val alternativesWithDistance =
        findViews(tpe)
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
      case I.Ignored(_, _) =>
        parts(tpe, 0, 0, Set(), 1)
      case _ =>
        val itpe = I.Ignored(Covariant, tpe :: Nil)
        parts(itpe, 0, 0, Set(), 1)
    }
  }
}

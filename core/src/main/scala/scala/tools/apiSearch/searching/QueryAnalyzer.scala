package scala.tools.apiSearch.searching

import scala.tools.apiSearch.model._
import scala.tools.apiSearch.index.ClassIndex
import scala.util.Try
import scalaz.ValidationNel
import scalaz.Validation.FlatMap._
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scala.tools.apiSearch.settings.QuerySettings

private[searching] sealed trait ResolvedQuery
private[searching] object ResolvedQuery {
  case class Class(cls: ClassEntity, args: List[ResolvedQuery]) extends ResolvedQuery
  case class TypeParam(name: String) extends ResolvedQuery
}

private[searching] case class FlattenedQuery(types: List[List[FlattenedQuery.Type]])
private[searching] object FlattenedQuery {
  case class Type(variance: Variance, typeName: String, distance: Int, depth: Int)
}

case class APIQuery(types: List[APIQuery.Type]) {
  def fingerprint: List[String] =
    for {
      tpe <- types
    } yield s"${tpe.variance.prefix}${tpe.typeName}_${tpe.occurrence}"
}
object APIQuery {
  case class Type(variance: Variance, typeName: String, occurrence: Int, boost: Float)
}

object QueryAnalyzer {
  sealed trait Error
  case class NameNotFound(part: RawQuery.Type) extends Error
  case class NameAmbiguous(part: RawQuery.Type, candidates: Seq[ClassEntity]) extends Error
  case class IllegalNumberOfTypeArgs(part: RawQuery.Type, expectedArgs: Int) extends Error

  type ErrorsOr[T] = ValidationNel[Error, T]

  def apply(settings: QuerySettings, classes: ClassIndex) =
    new QueryAnalyzer(settings, classes.findClass _, classes.findSubClasses _)
}

/**
 *
 */
class QueryAnalyzer private[searching] (
  settings: QuerySettings,
  findClass: (String) => Try[Seq[ClassEntity]],
  findSubClasses: (ClassEntity) => Try[Seq[ClassEntity]]) {

  import QueryAnalyzer._

  def apply(raw: RawQuery): Try[ErrorsOr[APIQuery]] =
    Try {
      resolveNames(raw.tpe).get.map(
        (normalizeFunctions _) andThen (flattenQuery _) andThen (_.get) andThen (toApiQuery _))
    }

  private[searching] def resolveNames(raw: RawQuery.Type): Try[ErrorsOr[ResolvedQuery]] =
    Try {
      val resolvedArgs: ErrorsOr[List[ResolvedQuery]] =
        raw.args.map(arg => resolveNames(arg).get).sequenceU

      resolvedArgs.flatMap { resolvedArgs =>
        filterFavored(findClass(raw.name).get) match {
          case Seq() if isTypeParam(raw.name) =>
            ResolvedQuery.TypeParam(raw.name).successNel
          case Seq() =>
            NameNotFound(raw).failureNel
          case Seq(cls) if resolvedArgs.length == cls.typeParameters.length || raw.args.length == 0 =>
            ResolvedQuery.Class(cls, resolvedArgs).successNel
          case Seq(cls) =>
            IllegalNumberOfTypeArgs(raw, cls.typeParameters.length).failureNel
          case candidates =>
            NameAmbiguous(raw, candidates).failureNel
        }
      }
    }

  private def isTypeParam(name: String): Boolean =
    name.length() == 1

  private def filterFavored(candidates: Seq[ClassEntity]): Seq[ClassEntity] = {
    // classes in root `scala` namespace and java.lang.String are always favored
    val firstPrioPattern = """(scala\.([^\.#]+))|java\.lang\.String"""
    // unambiguous names from the `scala` namespace are also priotized over names from other namespaces
    val secondPrioPattern = """scala\..*"""

    candidates.filter(_.name.matches(firstPrioPattern)) match {
      case Seq(fav) => Seq(fav)
      case _ => candidates.filter(_.name.matches(secondPrioPattern)) match {
        case Seq(fav) => Seq(fav)
        case _        => candidates
      }
    }
  }

  private[searching] def normalizeFunctions(resolved: ResolvedQuery): ResolvedQuery = {
    def uncurry(rq: ResolvedQuery, args: List[ResolvedQuery]): ResolvedQuery =
      rq match {
        case ResolvedQuery.Class(cls, functionArgs) if cls.isFunction =>
          uncurry(functionArgs.last, args ::: functionArgs.init)
        case _ =>
          val typeParams = args.map(_ => TypeParameterEntity("A", Contravariant)) :+ TypeParameterEntity("R", Covariant)
          // this is not a complete ClassEntity but it shouldn't matter as it gets elided
          // in later processing steps
          val function = ClassEntity(TypeEntity.functionType(args.length), typeParams, Nil)
          ResolvedQuery.Class(function, args :+ rq)
      }

    uncurry(resolved, Nil)
  }

  private[searching] def flattenQuery(resolved: ResolvedQuery): Try[FlattenedQuery] =
    Try {
      def flattenWithVarianceAndDepth(variance: Variance, depth: Int, rq: ResolvedQuery): List[(Variance, Int, ClassEntity)] =
        rq match {
          case ResolvedQuery.Class(cls, args) if depth == -1 && cls.isFunction =>
            cls.typeParameters.zip(args).flatMap {
              case (param, arg) => flattenWithVarianceAndDepth(param.variance * variance, depth + 1, arg)
            }
          case ResolvedQuery.Class(cls, args) =>
            val argParts = cls.typeParameters.zip(args).flatMap {
              case (param, arg) => flattenWithVarianceAndDepth(param.variance * variance, depth + 1, arg)
            }
            (variance, depth, cls) :: argParts
          case ResolvedQuery.TypeParam(_) => Nil
        }

      def withAlternatives(variance: Variance, depth: Int, cls: ClassEntity): List[FlattenedQuery.Type] = {
        val alternativesWithDistance: List[(String, Int)] = variance match {
          case Covariant => findSubClasses(cls).get
            .map(subCls => (subCls.name, subCls.baseTypes.indexWhere(_.name == cls.name) + 1)).toList
          case Contravariant => cls.baseTypes.map(_.name).zipWithIndex.map { case (name, idx) => (name, idx + 1) }
          case Invariant     => Nil
        }

        ((cls.name, 0) :: alternativesWithDistance).map {
          case (alt, distance) => FlattenedQuery.Type(variance, alt, distance, depth)
        }
      }

      val types = flattenWithVarianceAndDepth(Covariant, -1, resolved)
        .map((withAlternatives _).tupled)

      FlattenedQuery(types)
    }

  private[searching] def toApiQuery(flattened: FlattenedQuery): APIQuery = {
    val boostsPerType: Map[(Variance, String), List[Float]] =
      flattened.types.flatten.foldLeft(Map[(Variance, String), List[Float]]()) {
        (boostsPerType, tpe) =>
          val key = (tpe.variance, tpe.typeName)
          val boosts = boostsPerType.get(key).getOrElse(Nil)

          boostsPerType + (key -> (boost(tpe) :: boosts))
      }

    val orderedBoostsPerType = boostsPerType.mapValues(_.sorted(Ordering[Float].reverse))

    val types = for {
      ((variance, tpe), distances) <- orderedBoostsPerType
      (boost, idx) <- distances.zipWithIndex
    } yield APIQuery.Type(variance, tpe, idx, boost)

    APIQuery(types.toList.sortBy(-_.boost))
  }

  private def boost(tpe: FlattenedQuery.Type): Float =
    distanceBoost(tpe.distance) * depthBoost(tpe.depth)

  private def distanceBoost(dist: Int): Float = (1f / (settings.distanceBoostGradient * dist + 1f))
  private def depthBoost(depth: Int): Float = (1f / (settings.depthBoostGradient * depth + 1f))
}

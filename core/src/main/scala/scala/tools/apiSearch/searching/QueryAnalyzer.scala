package scala.tools.apiSearch.searching

import scala.tools.apiSearch.model._
import scala.tools.apiSearch.index.ClassIndex
import scala.util.Try

case class Suggestion(part: RawQuery.Type, candidates: Seq[ClassEntity])

sealed trait ResolvedQuery
case class ResolvedClass(cls: ClassEntity, args: List[ResolvedQuery]) extends ResolvedQuery
case class ResolvedTypeParam(name: String) extends ResolvedQuery

case class FlattenedQuery(types: List[List[FlattenedQuery.Type]])
object FlattenedQuery {
  case class Type(variance: Variance, typeName: String, distance: Int)
}

case class APIQuery(types: List[APIQuery.Type]) {
  def fingerprint: List[String] =
    for {
      tpe <- types
    } yield s"${tpe.variance.prefix}${tpe.typeName}_${tpe.occurrence}"
}
object APIQuery {
  case class Type(variance: Variance, typeName: String, occurrence: Int, distance: Int)
}

/**
 *
 */
class QueryAnalyzer(
  findClass: (String) => Try[Seq[ClassEntity]],
  findSubClasses: (ClassEntity) => Try[Seq[ClassEntity]]) {

  def apply(raw: RawQuery): Try[Either[Suggestion, APIQuery]] =
    Try {
      resolveNames(raw.tpe).get.right.map { resolved =>
        // TODO: normalize distances?
        toApiQuery(flattenQuery(resolved).get)
      }
    }

  def resolveNames(raw: RawQuery.Type): Try[Either[Suggestion, ResolvedQuery]] =
    Try {
      val suggestionOrResolvedArgs = raw.args.foldLeft[Either[Suggestion, List[ResolvedQuery]]](Right(Nil)) { (acc, arg) =>
        (acc, resolveNames(arg).get) match {
          case (Right(qs), Right(q)) => Right(qs :+ q)
          case (Left(s), _)          => Left(s)
          case (_, Left(s))          => Left(s)
        }
      }

      suggestionOrResolvedArgs.right.flatMap { resolvedArgs =>
        findClass(raw.name).get match {
          case Seq() if isTypeParam(raw.name) =>
            Right(ResolvedTypeParam(raw.name))
          case Seq(cls) if raw.args.length == cls.typeParameters.length || raw.args.length == 0 =>
            Right(ResolvedClass(cls, resolvedArgs))
          case candidates =>
            favoredCandidate(candidates)
              .fold[Either[Suggestion, ResolvedQuery]](Left(Suggestion(raw, candidates))) {
                cls => Right(ResolvedClass(cls, resolvedArgs))
              }
        }
      }
    }

  private def isTypeParam(name: String): Boolean =
    name.length() == 1

  private def favoredCandidate(candidates: Seq[ClassEntity]): Option[ClassEntity] = {
    // classes in root `scala` namespace and java.lang.String are always favored
    val firstPrioPattern = """(scala\.([^\.#]+))|java\.lang\.String"""
    // unambiguous names from the `scala` namespace are also priotized over names from other namespaces
    val secondPrioPattern = """scala\..*"""

    candidates.find(_.name.matches(firstPrioPattern)).orElse {
      candidates.filter(_.name.matches(secondPrioPattern)) match {
        case Seq(fav) => Some(fav)
        case _        => None
      }
    }
  }

  def flattenQuery(resolved: ResolvedQuery): Try[FlattenedQuery] =
    Try {
      def flattenWithVariance(variance: Variance, rq: ResolvedQuery): List[(Variance, ClassEntity)] = rq match {
        case ResolvedClass(cls, args) =>
          val argParts = cls.typeParameters.zip(args).flatMap {
            case (param, arg) => flattenWithVariance(param.variance * variance, arg)
          }
          (variance, cls) :: argParts
        case ResolvedTypeParam(_) => Nil
      }

      def withAlternatives(variance: Variance, cls: ClassEntity): List[FlattenedQuery.Type] = {
        val alternativesWithDistance: List[(String, Int)] = variance match {
          case Covariant     => cls.baseTypes.map(_.name).zipWithIndex.map { case (name, idx) => (name, idx + 1) }
          case Contravariant => findSubClasses(cls).get.map(subCls => (subCls.name, 1)).toList
          case Invariant     => Nil
        }

        ((cls.name, 0) :: alternativesWithDistance).map {
          case (alt, distance) => FlattenedQuery.Type(variance, alt, distance)
        }
      }

      val types = flattenWithVariance(Covariant, resolved)
        .map((withAlternatives _).tupled)

      FlattenedQuery(types)
    }

  def toApiQuery(flattened: FlattenedQuery): APIQuery = {
    val distancesPerType: Map[(Variance, String), List[Int]] =
      flattened.types.flatten.foldLeft(Map[(Variance, String), List[Int]]()) {
        (distancesPerType, tpe) =>
          val key = (tpe.variance, tpe.typeName)
          val distances = distancesPerType.get(key).getOrElse(Nil)

          distancesPerType + (key -> (tpe.distance :: distances))
      }

    val orderedDistancesPerType = distancesPerType.mapValues(_.sorted(Ordering[Int].reverse))

    val types = for {
      ((variance, tpe), distances) <- orderedDistancesPerType
      (distance, idx) <- distances.zipWithIndex
    } yield APIQuery.Type(variance, tpe, idx, distance)

    APIQuery(types.toList.sortBy(_.distance))
  }
}

object QueryAnalyzer {
  def apply(classes: ClassIndex) =
    new QueryAnalyzer(classes.findClass _, classes.findSubClasses _)
}

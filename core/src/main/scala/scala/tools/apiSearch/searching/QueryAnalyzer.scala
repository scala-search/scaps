package scala.tools.apiSearch.searching

import scala.tools.apiSearch.model._
import scala.tools.apiSearch.index.ClassIndex
import scala.util.Try

case class Suggestion(part: RawQuery, candidates: Seq[ClassEntity])

sealed trait ResolvedQuery
case class ResolvedClass(cls: ClassEntity, args: List[ResolvedQuery]) extends ResolvedQuery
case class ResolvedTypeParam(name: String) extends ResolvedQuery

case class APIQuery(types: List[QueryType]) {
  def fingerprint: List[String] =
    for {
      tpe <- types
      alternative <- tpe.typeNames
    } yield s"${tpe.variance.prefix}${alternative}_${tpe.occurrence}"
}
case class QueryType(variance: Variance, typeNames: List[String], occurrence: Int)

/**
 *
 */
class QueryAnalyzer(
  findClass: (String) => Try[Seq[ClassEntity]],
  findSubClasses: (ClassEntity) => Try[Seq[ClassEntity]]) {

  def apply(raw: RawQuery): Try[Either[Suggestion, APIQuery]] =
    Try {
      val resolved = resolveNames(raw).get

      resolved.right.map(rq => createQuery(rq).get)
    }

  private def createQuery(resolvedQuery: ResolvedQuery): Try[APIQuery] =
    createTypes(Covariant, resolvedQuery).map { types =>
      val typesWithOcc = types.groupBy(identity)
        .flatMap { case (tpe, values) => values.zipWithIndex.map { case (_, idx) => tpe.copy(occurrence = idx + 1) } }

      APIQuery(typesWithOcc.toList)
    }

  private def createTypes(variance: Variance, query: ResolvedQuery): Try[List[QueryType]] =
    Try {
      def toPart(cls: ClassEntity) = {
        val alternatives = variance match {
          case Contravariant => cls.baseTypes.map(_.name)
          case Covariant     => findSubClasses(cls).get.map(_.name)
          case _             => Nil
        }
        QueryType(variance, cls.name :: alternatives.toList, 0)
      }

      query match {
        case ResolvedClass(cls, Nil) =>
          toPart(cls) :: Nil
        case ResolvedClass(cls, args) =>
          val argParts = cls.typeParameters.zip(args).flatMap {
            case (param, arg) => createTypes(param.variance * variance, arg).get
          }
          toPart(cls) :: argParts
        case ResolvedTypeParam(_) =>
          Nil
      }
    }

  def resolveNames(raw: RawQuery): Try[Either[Suggestion, ResolvedQuery]] =
    Try {
      val suggestionOrResolvedArgs = raw.args.foldLeft[Either[Suggestion, List[ResolvedQuery]]](Right(Nil)) { (acc, arg) =>
        (acc, resolveNames(arg).get) match {
          case (Right(qs), Right(q)) => Right(qs :+ q)
          case (Left(s), _)          => Left(s)
          case (_, Left(s))          => Left(s)
        }
      }

      suggestionOrResolvedArgs.right.flatMap { resolvedArgs =>
        findClass(raw.tpe).get match {
          case Seq() if isTypeParam(raw.tpe) =>
            Right(ResolvedTypeParam(raw.tpe))
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

  private def favoredCandidate(candidates: Seq[ClassEntity]): Option[ClassEntity] =
    candidates.filter(c => c.name.startsWith("scala.")) match {
      case Seq(fav) => Some(fav)
      case _        => None
    }
}

object QueryAnalyzer {
  def apply(classes: ClassIndex) =
    new QueryAnalyzer(classes.findClass _, classes.findSubClasses _)
}

package scala.tools.apiSearch.searching

import scala.tools.apiSearch.model._
import scala.tools.apiSearch.index.ClassIndex
import scala.util.Try

case class Suggestion(part: RawQuery, candidates: Seq[ClassEntity])

sealed trait ResolvedQuery
case class ResolvedClass(cls: ClassEntity, args: List[ResolvedQuery]) extends ResolvedQuery
case class ResolvedTypeParam(name: String) extends ResolvedQuery

case class APIQuery(parts: List[Part])
case class Part(variance: Variance, alternatives: List[String])

/**
 *
 */
class QueryAnalyzer(
  findClass: (String) => Try[Seq[ClassEntity]],
  findSubClasses: (String) => Try[Seq[ClassEntity]]) {

  def apply(raw: RawQuery): Try[Either[Suggestion, APIQuery]] =
    Try {
      val resolved = resolveNames(raw).get

      resolved.right.map(rq => APIQuery(createParts(Covariant, rq).get))
    }

  private def createParts(variance: Variance, query: ResolvedQuery): Try[List[Part]] =
    Try {
      def toPart(cls: ClassEntity) = {
        val alternatives = variance match {
          case Contravariant => cls.baseTypes.map(_.name)
          case Covariant     => findSubClasses(cls.name).get.map(_.name)
          case _             => Nil
        }
        Part(variance, cls.name :: alternatives.toList)
      }

      query match {
        case ResolvedClass(cls, Nil) =>
          toPart(cls) :: Nil
        case ResolvedClass(cls, args) =>
          val argParts = cls.typeParameters.zip(args).flatMap {
            case (param, arg) => createParts(param.variance * variance, arg).get
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

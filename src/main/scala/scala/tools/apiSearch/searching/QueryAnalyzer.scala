package scala.tools.apiSearch.searching

import scala.tools.apiSearch.model._
import scala.tools.apiSearch.index.ClassIndex
import scala.util.Try

case class Suggestion(part: RawQuery, candidates: Seq[ClassEntity])

case class ResolvedQuery(cls: ClassEntity, args: List[ResolvedQuery])

case class APIQuery(parts: List[Part])
case class Part(variance: Variance, alternatives: List[String])

/**
 *
 */
class QueryAnalyzer(findClass: (String, Int) => Try[Seq[ClassEntity]]) {
  def apply(raw: RawQuery): Try[Either[Suggestion, APIQuery]] =
    Try {
      val resolved = resolveNames(raw).get

      resolved.right.map(rq => APIQuery(createParts(Covariant, rq).get))
    }

  private def createParts(variance: Variance, query: ResolvedQuery): Try[List[Part]] =
    Try {
      query match {
        case ResolvedQuery(cls, Nil) => Part(variance, cls.name :: Nil) :: Nil
        case ResolvedQuery(cls, args) =>
          val argParts = cls.typeParameters.zip(args).flatMap {
            case (param, arg) => createParts(param.variance * variance, arg).get
          }

          Part(variance, cls.name :: Nil) :: argParts
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
        findClass(raw.tpe, raw.args.length).get match {
          case Seq(cls) =>
            Right(ResolvedQuery(cls, resolvedArgs))
          case candidates =>
            Left(Suggestion(raw, candidates))
        }
      }
    }

  private def substitute(cls: ClassEntity, param: TypeParameterEntity, replacement: TypeEntity): ClassEntity = {
    val params = cls.typeParameters.filterNot(_ == param)
    val baseTypes = cls.baseTypes.map(substitute(_, param, replacement))

    cls.copy(typeParameters = params, baseTypes = baseTypes)
  }

  private def substitute(tpe: TypeEntity, param: TypeParameterEntity, replacement: TypeEntity): TypeEntity = {
    if (tpe.name == param.name)
      replacement
    else
      tpe.copy(args = tpe.args.map(substitute(_, param, replacement)))
  }
}

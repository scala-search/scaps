package scala.tools.apiSearch.searching

import scala.tools.apiSearch.model._
import scala.tools.apiSearch.index.ClassIndex
import scala.util.Try

case class Query(types: List[(Variance, ClassEntity)])

case class Suggestion(tpe: String, candidates: List[ClassEntity])

/**
 *
 */
class QueryAnalyzer(findClass: (String, Int) => Try[List[ClassEntity]]) {
  def apply(raw: RawQuery): Either[List[Suggestion], Query] = {
    ???
  }

  def resolveTypes(raw: RawQuery): Try[Either[Suggestion, RawQuery]] =
    Try {
      val newArgs = raw.args.foldLeft[Either[Suggestion, List[RawQuery]]](Right(Nil)) { (acc, arg) =>
        (acc, resolveTypes(arg).get) match {
          case (Right(qs), Right(q)) => Right(qs :+ q)
          case (Left(s), _)          => Left(s)
          case (_, Left(s))          => Left(s)
        }
      }

      newArgs.right.flatMap { newArgs =>
        findClass(raw.tpe, raw.args.length).get match {
          case cls :: Nil =>
            Right(raw.copy(tpe = cls.name, args = newArgs, cls = Some(cls)))
          case candidates =>
            Left(Suggestion(raw.tpe, candidates))
        }
      }
    }

  def toQuery(raw: RawQuery, variance: Variance): List[(Variance, ClassEntity)] = ???

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

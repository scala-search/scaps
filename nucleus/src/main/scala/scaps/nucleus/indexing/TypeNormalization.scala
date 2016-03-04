package scaps.nucleus.indexing

import java.util.regex.Pattern
import scaps.nucleus.Contravariant
import scaps.nucleus.Covariant
import scaps.nucleus.Invariant
import scaps.nucleus.LanguageSettings
import scaps.nucleus.TypeParam
import scaps.nucleus.TypeRef
import scaps.nucleus.Variance
import scaps.nucleus.Type

private[nucleus] object TypeNormalization {
  import scaps.nucleus.indexing.{ InternalTypes => I }

  def substituteTypeParams(tpe: Type): TypeRef = {
    def loop(tr: TypeRef): TypeRef =
      tr match {
        case t @ TypeRef(v, name, args) =>
          tpe.params.find(_.name == name)
            .map { tp =>
              v match {
                case Covariant =>
                  tp.lowerBound
                    .getOrElse(I.Bottom(v, args))
                case Contravariant =>
                  tp.upperBound
                    .getOrElse(I.Top(v, args))
                case Invariant =>
                  I.Unknown(v, args)
              }
            }
            .getOrElse(t.copy(args = args.map(loop)))
        case t => t.copy(args = t.args.map(loop))
      }

    loop(tpe.ref)
  }

  def normalize(tpe: TypeRef): TypeRef = {
    def curryFunctions(tpe: TypeRef): TypeRef =
      tpe match {
        case I.Fn(v, a :: (as @ (_ :: _)), res) =>
          I.Fn(v, List(a), curryFunctions(I.Fn(v, as, res)))
        case t => t.copy(args = t.args.map(curryFunctions))
      }

    def uncurryOutermostFunctionApplications(tpe: TypeRef): TypeRef = {
      def outermostArgsAndResult(tpe: TypeRef, prevArgs: List[TypeRef] = Nil): (List[TypeRef], TypeRef) =
        tpe match {
          case I.Fn(Covariant, args, res) =>
            outermostArgsAndResult(res, prevArgs ++ args)
          case t => (prevArgs, t)
        }

      val (args, res) = outermostArgsAndResult(tpe)

      if (args.isEmpty)
        res
      else
        I.Fn(tpe.variance, args, res)
    }

    val normalize =
      (curryFunctions _) andThen
        (uncurryOutermostFunctionApplications _)

    normalize(tpe)
  }

  def renameTypeParams(paramsWithNewName: List[(TypeParam, String)], tpe: TypeRef): TypeRef =
    tpe match {
      case t @ TypeRef(v, name, args) =>
        val renamedArgs = args.map(renameTypeParams(paramsWithNewName, _))

        paramsWithNewName.find(_._1.name == name).fold {
          t.copy(args = renamedArgs)
        } {
          case (_, newName) =>
            TypeRef(v, newName, renamedArgs)
        }
    }
}

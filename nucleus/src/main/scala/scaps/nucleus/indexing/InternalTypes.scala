package scaps.nucleus.indexing

import scaps.nucleus._
import java.util.regex.Pattern

private[nucleus] object InternalTypes {

  def toInternal(d: Definition, language: LanguageSettings): Definition = d match {
    case v: ValueDef => toInternal(v, language)
    case t: TypeDef  => toInternal(t, language)
  }

  def toInternal(v: ValueDef, language: LanguageSettings): ValueDef = {
    val (tps, tpe) = toInternal(v.typeParameters, v.tpe, language)
    v.copy(typeParameters = tps, tpe = tpe)
  }

  def toInternal(td: TypeDef, language: LanguageSettings): TypeDef = {
    val (tps, tpe) = toInternal(td.typeParameters, td.tpe, language)
    val supers = td.supertypes.map(sup => toInternal(td.typeParameters, sup, language)._2)
    td.copy(typeParameters = tps, tpe = tpe, supertypes = supers)
  }

  def toInternal(tpe: TypeRef, language: LanguageSettings): TypeRef =
    toInternal(Nil, tpe, language)._2

  def toInternal(params: List[TypeParam], tpe: TypeRef, language: LanguageSettings): (List[TypeParam], TypeRef) = {
    class RegexTypeExtractor(p: Pattern) {
      def unapply(t: TypeRef): Option[(Variance, List[TypeRef])] = t match {
        case TypeRef(v, name, args) if p.matcher(name).matches() =>
          Some((v, args))
        case _ => None
      }
    }

    object NativeFn extends RegexTypeExtractor(language.functionTypePattern)
    object NativeTop extends RegexTypeExtractor(language.topTypePattern)
    object NativeBottom extends RegexTypeExtractor(language.bottomTypePattern)

    def unifyFunctionCalls(tpe: TypeRef): TypeRef = {
      val argsWithRes = tpe match {
        case NativeFn(_, args) =>
          args.lastOption.map(res => (args.init, res))
        case _ => None
      }

      argsWithRes
        .map {
          case (args, res) =>
            InternalTypes.Fn(tpe.variance, args, res)
        }
        .getOrElse(tpe)
    }

    def unifyTopAndBottom(tpe: TypeRef): TypeRef = tpe match {
      case NativeTop(v, args)    => InternalTypes.Top(v, args)
      case NativeBottom(v, args) => InternalTypes.Bottom(v, args)
      case t                     => t
    }

    val unify =
      (unifyTopAndBottom _) andThen
        (unifyFunctionCalls _)

    def unifyTopDown(tpe: TypeRef): TypeRef = {
      val normalized = unify(tpe)
      normalized.copy(args = normalized.args.map(unifyTopDown))
    }

    val internalParams = params.map(tp =>
      tp.copy(
        lowerBound = tp.lowerBound.map(unifyTopDown),
        upperBound = tp.upperBound.map(unifyTopDown)))

    val internalTpe = unifyTopDown(tpe)

    (internalParams, internalTpe)
  }

  class Type(name: String) extends TypeRef(Invariant, name, Nil) {
    def apply(v: Variance): TypeRef =
      TypeRef(v, name, Nil)

    def unapply(t: TypeRef): Option[Variance] = t match {
      case TypeRef(v, `name`, Nil) => Some(v)
      case _                       => None
    }
  }

  class UnaryType(val name: String) {
    def apply(v: Variance, arg: TypeRef): TypeRef =
      TypeRef(v, name, List(arg))

    def apply(arg: TypeRef): TypeRef =
      apply(Invariant, arg)

    def unapply(t: TypeRef): Option[(Variance, List[TypeRef])] = t match {
      case TypeRef(v, `name`, List(arg)) =>
        Some((v, List(arg)))
      case _ => None
    }
  }

  object Optional extends UnaryType("<optional>")

  class GeneralType(val prefix: String, val suffix: String = ">") {
    def mkName(noArgs: Int) = s"$prefix$noArgs$suffix"

    def apply(v: Variance, args: List[TypeRef]): TypeRef =
      TypeRef(v, mkName(args.length), args)

    def apply(args: TypeRef*): TypeRef =
      apply(Invariant, args.toList)

    def unapply(t: TypeRef): Option[(Variance, List[TypeRef])] = t match {
      case TypeRef(v, name, args) if name == mkName(args.length) =>
        Some((v, args))
      case _ => None
    }
  }

  object __ extends GeneralType("_", "")
  object Ignored extends GeneralType("<ignored")
  object Top extends GeneralType("<top")
  object Bottom extends GeneralType("<bottom")
  object Unknown extends GeneralType("<unknown")

  class FunctionLikeType(val prefix: String, val suffix: String = ">") {
    def mkName(noArgs: Int) = s"$prefix$noArgs$suffix"

    def apply(v: Variance, args: List[TypeRef], res: TypeRef): TypeRef =
      TypeRef(v, mkName(args.length), args :+ res)

    def apply(firstArgOrRes: TypeRef, rest: TypeRef*): TypeRef = {
      val (args, List(res)) = (firstArgOrRes :: rest.toList).splitAt(rest.length)
      apply(Invariant, args, res)
    }

    def unapply(t: TypeRef): Option[(Variance, List[TypeRef], TypeRef)] = t match {
      case TypeRef(v, name, firstArgOrRes :: rest) if name == mkName(rest.length) =>
        val (args, List(res)) = (firstArgOrRes :: rest).splitAt(rest.length)
        Some((v, args, res))
      case _ => None
    }
  }

  object Fn extends FunctionLikeType("<fn")
}

package scaps.nucleus

object DefBuilder {
  import TestLanguage._

  def vall(tpe: TypeRef, isImplicit: Boolean = false) =
    ValueDef("", Nil, tpe, isImplicit, "")

  def deff(args: TypeRef*)(res: TypeRef, isImplicit: Boolean = false) =
    ValueDef("", Nil, T.MethodInvocation(Covariant, args.toList, res), isImplicit, "")

  def extendss(tpe: TypeRef, base: TypeRef*) =
    TypeDef(Nil, tpe, base.toList, "")

  def tp(name: String) =
    TypeParam(name, None, None, None)

  def extendss(params: TypeParam*)(tpe: TypeRef, base: TypeRef*) =
    TypeDef(params.toList, tpe, base.toList, "")
}

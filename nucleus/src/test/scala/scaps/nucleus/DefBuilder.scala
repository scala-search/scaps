package scaps.nucleus

object DefBuilder {
  import TestLanguage._

  def vall(tpe: TypeRef, isImplicit: Boolean = false) =
    ValueDef("", Type(Nil, tpe), isImplicit, "")

  def deff(args: TypeRef*)(res: TypeRef, isImplicit: Boolean = false) =
    ValueDef("", Type(Nil, T.MethodInvocation(Covariant, args.toList, res)), isImplicit, "")

  def extendss(tpe: TypeRef, base: TypeRef*) =
    TypeDef(Type(Nil, tpe), base.toList, "")

  def tp(name: String) =
    TypeParam(name, None, None, None)

  def extendss(params: TypeParam*)(tpe: TypeRef, base: TypeRef*) =
    TypeDef(Type(params.toList, tpe), base.toList, "")
}

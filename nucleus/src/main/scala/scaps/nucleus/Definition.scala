package scaps.nucleus

sealed trait Definition {
  def typeParameters: List[TypeParam]
  def tpe: TypeRef
  def source: String
}

case class ValueDef(
    name: String,
    typeParameters: List[TypeParam],
    tpe: TypeRef,
    isImplicit: Boolean,
    source: String) extends Definition {
  assert(tpe.variance == Covariant)
}

case class TypeDef(
    typeParameters: List[TypeParam],
    tpe: TypeRef,
    supertypes: List[TypeRef],
    source: String) extends Definition {
  assert(tpe.variance == Covariant)
  assert(supertypes.forall(_.variance == Covariant))
}

case class TypeParam(
  name: String,
  variance: Option[Variance],
  lowerBound: Option[TypeRef],
  upperBound: Option[TypeRef])

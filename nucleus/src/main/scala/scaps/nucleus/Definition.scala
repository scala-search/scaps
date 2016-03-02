package scaps.nucleus

sealed trait Definition {
  def tpe: Type
  def source: String
}

case class ValueDef(
    name: String,
    tpe: Type,
    isImplicit: Boolean,
    source: String) extends Definition {
  assert(tpe.ref.variance == Covariant)
}

case class TypeDef(
    tpe: Type,
    supertypes: List[TypeRef],
    source: String) extends Definition {
  assert(tpe.ref.variance == Covariant)
  assert(supertypes.forall(_.variance == Covariant))
}

case class TypeParam(
  name: String,
  variance: Option[Variance],
  lowerBound: Option[TypeRef],
  upperBound: Option[TypeRef])

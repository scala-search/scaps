package scaps.nucleus

import scaps.nucleus.indexing.InternalTypes;

case class Type(params: List[TypeParam], ref: TypeRef) {
  def isTypeParam(tr: TypeRef) =
    params.exists(_.name == tr.name)
}

case class TypeRef(variance: Variance, name: String, args: List[TypeRef]) {
  def unary_+ = copy(variance = Covariant)
  def unary_- = copy(variance = Contravariant)
  def unary_~ = copy(variance = Invariant)

  override def toString = {
    val argsStr = if (args.isEmpty) "" else args.mkString("[", ", ", "]")
    s"${variance.prefix}$name$argsStr"
  }

  def atPosition(v: Variance): TypeRef = {
    if (v == variance)
      this
    else if (v == Invariant)
      copy(variance = Invariant, args = args.map(_.atPosition(Invariant)))
    else
      copy(variance = variance.flip, args = args.map(arg => arg.atPosition(arg.variance.flip)))
  }

  def apply(paramName: String, arg: TypeRef): TypeRef = {
    if (name == paramName)
      arg.atPosition(variance).copy(args = args.map(_.apply(paramName, arg)))
    else
      copy(args = args.map(_.apply(paramName, arg)))
  }

  def toList: List[TypeRef] = this :: args.flatMap(_.toList)
}

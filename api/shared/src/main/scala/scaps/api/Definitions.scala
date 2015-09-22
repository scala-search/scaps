package scaps.api

sealed trait Definition {
  def name: String

  def shortName: String =
    EntityName.splitName(name).last
}

case class TypeDef(
  name: String,
  typeParameters: List[TypeParameter],
  baseTypes: List[TypeRef],
  referencedFrom: Set[Module] = Set(),
  comment: String = "",
  typeFrequency: Map[Variance, Float] = Map())
  extends Definition {

  override def toString() = {
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    val bases = baseTypes.mkString("extends ", " with ", "")

    s"$name$params $bases"
  }

  def isFunction = typeParameters.length > 0 && name == TypeRef.Function.name(typeParameters.length - 1)

  def frequency(v: Variance) =
    typeFrequency.get(v).getOrElse(0f)

  def toType: TypeRef =
    TypeRef(name, Covariant, typeParameters.map(p => TypeRef(p.name, p.variance, Nil, true)))
}

case class ValueDef(
  name: String,
  typeParameters: List[TypeParameter],
  tpe: TypeRef,
  comment: String,
  flags: Set[ValueDef.Flag] = Set(),
  docLink: Option[String] = None,
  module: Module = Module.Unknown)
  extends Definition {

  override def toString() = {
    val c = comment match {
      case "" => ""
      case _  => s"$comment\n"
    }
    val mods = flags.map(_.name).mkString(" ")
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    s"$c$mods $name$params: $tpe"
  }

  /**
   * A unique description of the value including its name and type.
   */
  def signature: String = {
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    s"$name$params: ${tpe.signature}"
  }

  def typeFingerprint: List[String] =
    tpe.normalize(typeParameters).typeFingerprint

  def withoutComment = copy(comment = "")

  def isOverride = flags(ValueDef.Overrides)

  def isImplicit = flags(ValueDef.Implicit)

  def isStatic = flags(ValueDef.Static)
}

object ValueDef {
  sealed trait Flag {
    def name: String
  }
  case object Overrides extends Flag {
    val name = "overrides"
  }
  case object Implicit extends Flag {
    val name = "implicit"
  }
  case object Static extends Flag {
    val name = "static"
  }
}

case class TypeParameter(
  name: String,
  variance: Variance,
  lowerBound: String = TypeRef.Nothing.name,
  upperBound: String = TypeRef.Any.name) {

  import TypeRef._

  override def toString() = {
    val lbound =
      if (lowerBound == Nothing.name) ""
      else s" >: $lowerBound"

    val ubound =
      if (upperBound == Any.name) ""
      else s" <: $upperBound"

    s"$name$lbound$ubound"
  }
}

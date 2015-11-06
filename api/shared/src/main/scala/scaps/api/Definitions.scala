package scaps.api

sealed trait Definition {
  def name: String

  def shortName: String =
    EntityName.splitName(name).last

  def module: Module

  def withModule(m: Module): this.type = (this match {
    case t: TypeDef  => t.copy(module = m)
    case v: ValueDef => v.copy(module = m)
    case v: ViewDef  => v.copy(module = m)
  }).asInstanceOf[this.type]
}

case class TypeDef(
  name: String,
  typeParameters: List[TypeParameter],
  comment: String = "",
  module: Module = Module.Unknown,
  typeFrequency: Map[Variance, Float] = Map())
    extends Definition {

  override def toString() = {
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    s"$name$params"
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
  comment: DocComment,
  flags: Set[ValueDef.Flag] = Set(),
  module: Module = Module.Unknown,
  docLink: Option[String] = None)
    extends Definition {

  override def toString() = {
    val c = comment match {
      case DocComment.empty => ""
      case _                => s"$comment\n"
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

  def withoutComment = copy(comment = DocComment.empty)

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

case class ViewDef(from: TypeRef, to: TypeRef, distance: Float, definingEntityName: String, module: Module = Module.Unknown)
    extends Definition {
  def name = s"$definingEntityName:$fromKey:$toKey"

  def fromKey = ViewDef.key(from)
  def toKey = ViewDef.key(to)

  def apply(t: TypeRef): Option[TypeRef] = {
    val paramMap = findParamMap(from, t)
    Some(paramMap.foldLeft(to) { (t, paramWithArg) =>
      to(paramWithArg._1, paramWithArg._2)
    })
  }

  def findParamMap(from: TypeRef, t: TypeRef): List[(String, TypeRef)] =
    if (from.isTypeParam)
      List((from.name -> t))
    else
      from.args.zip(t.args).flatMap {
        case (f, t) => findParamMap(f, t)
      }
}

object ViewDef {
  def key(tpe: TypeRef) =
    tpe.renameTypeParams(_ => "_").annotatedSignature

  def bidirectional(from: TypeRef, to: TypeRef, distance: Float, definingEntityName: String) =
    List(
      ViewDef(from, to, distance, definingEntityName),
      ViewDef(to.withVariance(to.variance.flip), from.withVariance(from.variance.flip), distance, definingEntityName))
}

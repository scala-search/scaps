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
  comment: DocComment,
  flags: Set[ValueDef.Flag] = Set(),
  docLink: Option[String] = None,
  module: Module = Module.Unknown)
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

case class ViewDef(from: TypeRef, to: TypeRef, distance: Float, definingEntityName: String, modules: Set[Module] = Set())
    extends Definition {
  assert(from.variance == Covariant, s"$from is not covariant")
  assert(to.variance == Covariant, s"$to is not covariant")

  def name = s"$definingEntityName:$fromKey:$toKey"

  def fromKey = ViewDef.key(from)
  def toKey = ViewDef.key(to)
}

object ViewDef {
  def key(tpe: TypeRef) =
    tpe.renameTypeParams(_ => "_").signature

  private def fromTypeDef(cls: TypeDef): List[ViewDef] = {
    val toRepeated = {
      // create implicit conversions from Seq and subtypes thereof to repeated args
      if (cls.name == TypeRef.Seq.name) {
        val p = cls.typeParameters(0)
        Seq(ViewDef(cls.toType, TypeRef.Repeated(TypeRef(p.name, Covariant, Nil, true)), implicitConversionDistance, ""))
      } else {
        cls.baseTypes.collect {
          case TypeRef.Seq(t, _) =>
            ViewDef(cls.toType, TypeRef.Repeated(t), implicitConversionDistance, "")
        }
      }
    }

    cls.baseTypes.zipWithIndex.flatMap {
      case (base, idx) =>
        Seq(
          ViewDef(cls.toType, base, idx + 1, cls.name))
    } ++ toRepeated
  }

  private def fromValue(t: ValueDef): List[ViewDef] = {
    if (t.isImplicit && t.isStatic) {
      t.tpe.withoutImplicitParams.etaExpanded match {
        case TypeRef.Function(from :: Nil, to, _) =>
          List(ViewDef(from.withVariance(Covariant), to, implicitConversionDistance, t.name))
        case _ =>
          Nil
      }
    } else
      Nil
  }

  val implicitConversionDistance = 0.5f

  def fromEntity(e: Definition): List[ViewDef] = e match {
    case c: TypeDef  => fromTypeDef(c)
    case t: ValueDef => fromValue(t)
    case _           => List()
  }
}

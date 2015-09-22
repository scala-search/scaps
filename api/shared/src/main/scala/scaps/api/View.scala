package scaps.api

sealed trait View {
  import View._

  def from: TypeRef
  def to: TypeRef

  assert(from.variance == Covariant, s"$from is not covariant")
  assert(to.variance == Covariant, s"$to is not covariant")

  def fromKey = key(from)
  def toKey = key(to)

  def distance: Float
}

object View {
  private def fromTypeDef(cls: TypeDef): Seq[View] = {
    val toRepeated = {
      // create implicit conversions from Seq and subtypes thereof to repeated args
      if (cls.name == TypeRef.Seq.name) {
        val p = cls.typeParameters(0)
        Seq(ImplicitConversion(cls.toType, TypeRef.Repeated(TypeRef(p.name, Covariant, Nil, true)), ""))
      } else {
        cls.baseTypes.collect {
          case TypeRef.Seq(t, _) =>
            ImplicitConversion(cls.toType, TypeRef.Repeated(t), "")
        }
      }
    }

    cls.baseTypes.zipWithIndex.flatMap {
      case (base, idx) =>
        Seq(
          SubType(cls.toType, base, idx + 1))
    } ++ toRepeated
  }

  private def fromValue(t: ValueDef): Seq[View] = {
    if (t.isImplicit && t.isStatic) {
      t.tpe.withoutImplicitParams.etaExpanded match {
        case TypeRef.Function(from :: Nil, to, _) =>
          Seq(ImplicitConversion(from.withVariance(Covariant), to, t.name))
        case _ =>
          Nil
      }
    } else
      Nil
  }

  def fromEntity(e: Definition): Seq[View] = e match {
    case c: TypeDef  => fromTypeDef(c)
    case t: ValueDef => fromValue(t)
  }

  def key(tpe: TypeRef) =
    tpe.renameTypeParams(_ => "_").signature
}

case class SubType(cls: TypeRef, baseType: TypeRef, distance: Float) extends View {
  def from = cls
  def to = baseType

  override def toString =
    s"$fromKey is subtype of $toKey ($distance)"
}

case class ImplicitConversion(from: TypeRef, to: TypeRef, evidence: String) extends View {
  def distance = 0.5f

  override def toString =
    s"$fromKey is convertible to $toKey ($evidence)"
}

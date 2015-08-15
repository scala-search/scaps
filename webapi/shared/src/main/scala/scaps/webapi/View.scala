package scaps.webapi

sealed trait View {
  import View._

  def from: TypeEntity
  def to: TypeEntity

  assert(from.variance == Covariant, s"$from is not covariant")
  assert(to.variance == Covariant, s"$to is not covariant")

  def fromKey = key(from)
  def toKey = key(to)

  def distance: Float
}

object View {
  private def fromClass(cls: ClassEntity): Seq[View] = {
    val toRepeated = {
      if (cls.name == TypeEntity.Seq.name) {
        val p = cls.typeParameters(0)
        Seq(ImplicitConversion(cls.toType, TypeEntity.Repeated(TypeEntity(p.name, Covariant, Nil, true)), ""))
      } else {
        cls.baseTypes.collect {
          case TypeEntity.Seq(t, _) =>
            ImplicitConversion(cls.toType, TypeEntity.Repeated(t), "")
        }
      }
    }

    cls.baseTypes.zipWithIndex.flatMap {
      case (base, idx) =>
        Seq(
          SubType(cls.toType, base, idx + 1))
    } ++ toRepeated
  }

  private def fromTerm(t: TermEntity): Seq[View] = {
    if (t.isImplicit && t.isStatic) {
      t.tpe.withoutImplicitParams.etaExpanded match {
        case TypeEntity.Function(from :: Nil, to, _) =>
          Seq(ImplicitConversion(from.withVariance(Covariant), to, t.name))
        case _ =>
          Nil
      }
    } else
      Nil
  }

  def fromEntity(e: Entity): Seq[View] = e match {
    case c: ClassEntity => fromClass(c)
    case t: TermEntity  => fromTerm(t)
  }

  def key(tpe: TypeEntity) =
    tpe.renameTypeParams(_ => "_").signature
}

case class SubType(cls: TypeEntity, baseType: TypeEntity, distance: Float) extends View {
  def from = cls
  def to = baseType

  override def toString =
    s"$fromKey is subtype of $toKey ($distance)"
}

case class ImplicitConversion(from: TypeEntity, to: TypeEntity, evidence: String) extends View {
  def distance = 0.5f

  override def toString =
    s"$fromKey is convertible to $toKey ($evidence)"
}

package scaps.searchEngine

import scaps.webapi._

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
    cls.baseTypes.zipWithIndex.map {
      case (base, idx) => SubType(cls.toType, base, idx + 1)
    }
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

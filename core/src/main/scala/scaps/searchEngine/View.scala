package scaps.searchEngine

import scaps.webapi._

sealed trait View {
  import View._

  def from: TypeEntity
  def to: TypeEntity

  def fromKey = key(from)
  def toKey = key(to)

  def distance: Int
}

object View {
  def fromClass(cls: ClassEntity): Seq[View] = {
    val clsTpe = cls.toType

    val subTypes =
      cls.baseTypes.zipWithIndex.map {
        case (base, idx) => SubType(clsTpe, base, idx + 1)
      }

    if (cls.name == TypeEntity.Nothing)
      subTypes
    else
      SubType(TypeEntity.Nothing(Covariant), clsTpe, 1) :: subTypes
  }

  def key(tpe: TypeEntity) =
    tpe.renameTypeParams(_ => "_").signature
}

case class SubType(cls: TypeEntity, baseType: TypeEntity, distance: Int) extends View {
  def from = cls
  def to = baseType

  override def toString =
    s"$fromKey is subtype of $toKey ($distance)"
}

case class ImplicitConversion(from: TypeEntity, to: TypeEntity, evidence: String) extends View {
  def distance = 1

  override def toString =
    s"$fromKey converts implicitly to $toKey ($evidence)"
}

case class TypeClassImplementation(subject: TypeEntity, implementedClass: TypeEntity, evidence: String) extends View {
  def from = subject
  def to = implementedClass

  def distance = 1

  override def toString =
    s"$fromKey implements type class $toKey ($evidence)"
}

package scaps.searchEngine

import scaps.webapi._

sealed trait View {
  import View._

  def from: TypeEntity
  def to: TypeEntity

  def fromKey = key(from)
  def toKey = key(to)

  def distance: Option[Int]

  def relativeDistance(maxDistance: Int): Double =
    distance.fold(0.5) { dist => dist.toDouble / maxDistance }

  def otherEnd(fromOrTo: TypeEntity) =
    if (key(fromOrTo) == key(from))
      to
    else
      from
}

object View {
  def fromClass(cls: ClassEntity): Seq[View] = {
    val clsTpe = cls.toType
    
    cls.baseTypes.zipWithIndex.map {
      case (base, idx) => SubType(clsTpe, base, idx + 1)
    }
  }

  def key(tpe: TypeEntity) =
    tpe.renameTypeParams(_ => "_").signature
}

case class SubType(cls: TypeEntity, baseType: TypeEntity, dist: Int) extends View {
  def from = cls
  def to = baseType

  def distance = Some(dist)

  override def toString =
    s"$fromKey is subtype of $toKey ($dist)"
}

case class ImplicitConversion(from: TypeEntity, to: TypeEntity, evidence: String) extends View {
  def distance = None

  override def toString =
    s"$fromKey converts implicitly to $toKey ($evidence)"
}

case class TypeClassImplementation(subject: TypeEntity, implementedClass: TypeEntity, evidence: String) extends View {
  def from = subject
  def to = implementedClass

  def distance = None

  override def toString =
    s"$fromKey implements type class $toKey ($evidence)"
}

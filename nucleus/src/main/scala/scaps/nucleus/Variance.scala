package scaps.nucleus

sealed trait Variance {
  def prefix: String
  def flip: Variance
  def *(other: Variance): Variance
}
case object Invariant extends Variance {
  val prefix = "/"
  val flip = Invariant
  def *(other: Variance) = Invariant
}
case object Covariant extends Variance {
  val prefix = "+"
  val flip = Contravariant
  def *(other: Variance) = other
}
case object Contravariant extends Variance {
  val prefix = "-"
  val flip = Covariant
  def *(other: Variance) = other.flip
}

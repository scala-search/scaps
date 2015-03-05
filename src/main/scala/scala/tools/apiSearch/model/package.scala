package scala.tools.apiSearch

package object model {

  case class TermEntity(name: String, typeParameters: List[TypeParameterEntity], tpe: TypeEntity, comment: String) {
    override def toString() = {
      val c = comment match {
        case "" => ""
        case _  => s"$comment\n"
      }
      val params = typeParameters match {
        case Nil => ""
        case ps  => ps.mkString("[", ", ", "]")
      }
      s"$c$name$params: $tpe"
    }
  }

  case class TypeEntity(name: String, variance: Variance, args: List[TypeEntity] = Nil) {
    override def toString() = {
      val argStr = args match {
        case Nil => ""
        case as  => as.mkString("[", ", ", "]")
      }
      s"${variance.prefix}$name$argStr"
    }
  }

  object TypeEntity {
    val topTypeName = "scala.Any"
    val bottomTypeName = "scala.Nothing"
  }

  case class TypeParameterEntity(id: Int, lowerBound: String = TypeEntity.bottomTypeName, upperBound: String = TypeEntity.topTypeName) {
    import TypeEntity._

    val name = "$" + id

    override def toString() = {
      val lbound =
        if (lowerBound == bottomTypeName) ""
        else s" >: $lowerBound"

      val ubound =
        if (upperBound == topTypeName) ""
        else s" <: $upperBound"

      s"$name$lbound$ubound"
    }

    def m[T >: Nothing <: Any]: T = ???
  }

  sealed trait Variance {
    def prefix: String
    def flip: Variance
  }
  case object Invariant extends Variance {
    val prefix = ""
    val flip = Invariant
  }
  case object Covariant extends Variance {
    val prefix = "+"
    val flip = Contravariant
  }
  case object Contravariant extends Variance {
    val prefix = "-"
    val flip = Covariant
  }
}
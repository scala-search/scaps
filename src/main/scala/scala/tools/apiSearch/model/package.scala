package scala.tools.apiSearch

package object model {

  sealed trait Entity {
    def name: String
  }

  case class ClassEntity(name: String, typeParameters: List[TypeParameterEntity], baseTypes: List[TypeEntity])
    extends Entity {

    override def toString() = {
      val params = typeParameters match {
        case Nil => ""
        case ps  => ps.mkString("[", ", ", "]")
      }
      val bases = baseTypes.mkString("extends ", " with ", "")

      s"$name$params $bases"
    }
  }

  case class TermEntity(name: String, typeParameters: List[TypeParameterEntity], tpe: TypeEntity, comment: String)
    extends Entity {

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

    def fingerprint =
      tpe.fingerprintTypes(typeParameters)
        .map(tpe => s"${tpe.variance.prefix}${tpe.name}")
        .mkString(" ")
  }

  case class TypeEntity(name: String, variance: Variance = Covariant, args: List[TypeEntity] = Nil) {
    override def toString() = {
      val argStr = args match {
        case Nil => ""
        case as  => as.mkString("[", ", ", "]")
      }
      s"${variance.prefix}$name$argStr"
    }

    def fingerprintTypes(params: List[TypeParameterEntity]): List[TypeEntity] = {
      val paramOpt = params.find(_.name == name)
      val thisName = paramOpt.fold {
        name
      } { param =>
        if (variance == Contravariant)
          param.lowerBound
        else
          param.upperBound
      }
      copy(name = thisName) :: args.flatMap(_.fingerprintTypes(params))
    }
  }

  object TypeEntity {
    val topType = "scala.Any"
    val bottomType = "scala.Nothing"
  }

  case class TypeParameterEntity(name: String, lowerBound: String = TypeEntity.bottomType, upperBound: String = TypeEntity.topType) {
    import TypeEntity._

    override def toString() = {
      val lbound =
        if (lowerBound == bottomType) ""
        else s" >: $lowerBound"

      val ubound =
        if (upperBound == topType) ""
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

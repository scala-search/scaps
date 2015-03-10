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
      fingerprintTypes(tpe)
        .map(tpe => s"${tpe.variance.prefix}${tpe.name}")
        .mkString(" ")

    private def fingerprintTypes(tpe: TypeEntity): List[TypeEntity] = {
      val args = tpe.args.flatMap(fingerprintTypes)
      val paramOpt = typeParameters.find(_.name == tpe.name)
      paramOpt.fold {
        tpe :: args
      } { param =>
        if (param.upperBound != TypeEntity.topType)
          tpe.copy(name = param.upperBound) :: args
        else
          args
      }
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
    val topType = "scala.Any"
    val bottomType = "scala.Nothing"
    def functionType(n: Int) = s"scala.Function$n"
    def function(variance: Variance, paramTypes: List[TypeEntity], resultType: TypeEntity) = {
      val typeParams = paramTypes.map(pt => pt.copy(variance = variance.flip)) :+ resultType.copy(variance = variance)
      TypeEntity(functionType(paramTypes.length), variance, typeParams)
    }
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
    def *(other: Variance): Variance
  }
  case object Invariant extends Variance {
    val prefix = ""
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
}

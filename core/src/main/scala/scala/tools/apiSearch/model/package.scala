package scala.tools.apiSearch.model

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

  def isFunction = typeParameters.length > 0 && name == TypeEntity.functionType(typeParameters.length - 1)
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
      .groupBy(identity)
      .flatMap { case (typeName, values) => values.zipWithIndex.map { case (_, idx) => s"${typeName}_${idx}" } }
      .mkString(" ")

  private def fingerprintTypes(tpe: TypeEntity): List[TypeEntity] = {
    val args = tpe.args.flatMap(fingerprintTypes)

    // don't include member access, it is too common
    if (tpe.isMemberAccess || tpe.isMethodInvocation)
      args
    else {
      val paramOpt = typeParameters.find(_.name == tpe.name)
      paramOpt.fold {
        tpe :: args
      } { param =>
        // also type params with no upper bound provide too little information
        if (param.upperBound != TypeEntity.topType)
          tpe.copy(name = param.upperBound) :: args
        else
          args
      }
    }
  }

  def withoutComment = copy(comment = "")
}

case class TypeEntity(name: String, variance: Variance, args: List[TypeEntity]) {
  override def toString() = {
    val argStr = args match {
      case Nil => ""
      case as  => as.mkString("[", ", ", "]")
    }
    s"${variance.prefix}$name$argStr"
  }

  def isMemberAccess = name == TypeEntity.memberAccessType
  def isFunction = args.length > 0 && name == TypeEntity.functionType(args.length - 1)
  def isMethodInvocation = args.length > 0 && name == TypeEntity.methodInvocationType(args.length - 1)
}

object TypeEntity {
  val topType = "scala.Any"
  val bottomType = "scala.Nothing"

  def functionType(n: Int) = s"scala.Function$n"
  def function(variance: Variance, paramTypes: List[TypeEntity], resultType: TypeEntity) = {
    val typeArgs = paramTypes.map(pt => pt.copy(variance = variance.flip)) :+ resultType.copy(variance = variance)
    TypeEntity(functionType(paramTypes.length), variance, typeArgs)
  }

  def tupleType(n: Int) = s"scala.Tuple$n"

  val memberAccessType = "<memberAccess>"
  def memberAccess(owner: TypeEntity, member: TypeEntity): TypeEntity =
    TypeEntity(memberAccessType, Covariant, owner.copy(variance = Contravariant) :: member :: Nil)

  def methodInvocationType(n: Int) = s"<methodInvocation$n>"
  def methodInvocation(paramTypes: List[TypeEntity], resultType: TypeEntity) = {
    val typeArgs = paramTypes.map(_.copy(variance = Contravariant)) :+ resultType.copy(variance = Covariant)
    TypeEntity(methodInvocationType(paramTypes.length), Covariant, typeArgs)
  }

  def constructorType(n: Int) = s"<constructor$n>"
  def constructor(params: List[TypeEntity], result: TypeEntity) = {
    val typeArgs = params.map(_.copy(variance = Contravariant)) :+ result.copy(variance = Covariant)
    TypeEntity(constructorType(params.length), Covariant, typeArgs)
  }

  val unknownType = "<unknown>"
  val unknown = TypeEntity(unknownType, Covariant, Nil)

  def refinementType(n: Int) = s"<refinement$n>"
  def refinement(variance: Variance, args: List[TypeEntity]) = {
    val typeArgs = args.map(pt => pt.copy(variance = variance))
    TypeEntity(refinementType(typeArgs.length), variance, typeArgs)
  }

  def apply(name: String, args: List[TypeEntity] = Nil): TypeEntity =
    TypeEntity(name, Covariant, args)

  val any = TypeEntity(topType)
  val anyRef = TypeEntity("java.lang.Object")

  val int = TypeEntity("scala.Int")
}

case class TypeParameterEntity(name: String, variance: Variance, lowerBound: String = TypeEntity.bottomType, upperBound: String = TypeEntity.topType) {
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

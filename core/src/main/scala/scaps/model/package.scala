package scaps.model

import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean

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

  def isFunction = typeParameters.length > 0 && name == TypeEntity.Function.name(typeParameters.length - 1)
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

  /**
   * A unique description of the term including its name and type.
   */
  def signature: String = {
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    s"$name$params: ${tpe.signature}"
  }

  def fingerprint =
    Fingerprint(tpe.normalize(typeParameters).fingerprintTypes())

  def withoutComment = copy(comment = "")
}

case class TypeEntity(name: String, variance: Variance, args: List[TypeEntity]) {
  import TypeEntity._

  override def toString() = {
    val argStr = args match {
      case Nil => ""
      case as  => as.mkString("[", ", ", "]")
    }
    s"${variance.prefix}$name$argStr"
  }

  def signature: String = {
    val argStr = args match {
      case Nil => ""
      case as  => as.map(_.signature).mkString("[", ", ", "]")
    }
    s"$name$argStr"
  }

  def transform(f: TypeEntity => TypeEntity): TypeEntity =
    f(this.copy(args = args.map(_.transform(f))))

  def renameTypeParams(typeParams: List[TypeParameterEntity], getName: String => String): TypeEntity =
    transform { tpe =>
      if (typeParams.exists(_.name == tpe.name)) tpe.copy(name = getName(name))
      else tpe
    }

  def fingerprintTypes(depth: Int = 0): List[Fingerprint.Type] =
    this match {
      case TypeEntity.Ignored(args, _) =>
        args.flatMap(_.fingerprintTypes(depth + 1))
      case tpe =>
        Fingerprint.Type(tpe.variance, tpe.name, depth) :: tpe.args.flatMap(_.fingerprintTypes(depth + 1))
    }

  def normalize(typeParams: List[TypeParameterEntity] = Nil): TypeEntity = {
    def loop(tpe: TypeEntity): TypeEntity =
      tpe match {
        case MemberAccess(owner, member) =>
          loop(Function(owner :: Nil, loop(member)))
        case MethodInvocation(args, res, _) =>
          loop(Function(args, loop(res)))
        case Function(args1, Function(args2, res, _), v) =>
          // uncurry function
          loop(Function(args1 ::: args2, res, v))
        case Function(Tuple(args, _) :: Nil, res, v) =>
          // detuple function
          loop(Function(args, res, v))
        case Refinement(args, v) =>
          val newArgs = args.flatMap {
            case AnyRef(_) => None
            case arg       => Some(arg.copy(args = arg.args.map(loop)))
          }

          newArgs match {
            case arg :: Nil => arg
            case args       => Refinement(args, v)
          }
        case tpe: TypeEntity =>
          val normalizedArgs = tpe.args.map(loop)
          typeParams.find(_.name == tpe.name).fold {
            tpe.copy(args = normalizedArgs)
          } { param =>
            if (tpe.variance == Contravariant)
              tpe.copy(name = param.upperBound, args = normalizedArgs)
            else if (tpe.variance == Covariant)
              tpe.copy(name = param.lowerBound, args = normalizedArgs)
            else
              Unknown(tpe.variance)
          }
      }

    loop(this) match {
      case Function(args, res, v) =>
        Ignored(args :+ res, v)
      case tpe =>
        tpe
    }
  }
}

object TypeEntity {
  object Any extends PrimitiveType("scala.Any")
  object AnyRef extends PrimitiveType("java.lang.Object")
  object Int extends PrimitiveType("scala.Int")
  object String extends PrimitiveType("java.lang.String")
  object Nothing extends PrimitiveType("scala.Nothing")

  object Unknown extends PrimitiveType("<unknown>")

  class PrimitiveType(val name: String) {
    def apply(variance: Variance = Covariant) = TypeEntity(name, variance, Nil)

    def unapply(tpe: TypeEntity): Option[Variance] =
      (tpe.name == name && tpe.args.isEmpty).option(tpe.variance)
  }

  object Tuple extends VariantType("scala.Tuple")
  object Refinement extends VariantType("<refinement", ">")

  class VariantType(val tpePrefix: String, val tpeSuffix: String = "") {
    def name(n: Int) = s"$tpePrefix$n$tpeSuffix"

    def apply(args: List[TypeEntity], variance: Variance = Covariant) =
      TypeEntity(name(args.size), variance, args.map(pt => pt.copy(variance = variance)))

    def unapply(tpe: TypeEntity): Option[(List[TypeEntity], Variance)] =
      (!tpe.args.isEmpty && tpe.name == name(tpe.args.size)).option((tpe.args, tpe.variance))
  }

  object Function extends FunctionLikeType("scala.Function")
  object MethodInvocation extends FunctionLikeType("<methodInvocation", ">")

  class FunctionLikeType(val tpePrefix: String, val tpeSuffix: String = "") {
    def name(n: Int) = s"$tpePrefix$n$tpeSuffix"

    def apply(paramTypes: List[TypeEntity], resultType: TypeEntity, variance: Variance = Covariant) = {
      val typeArgs = paramTypes.map(pt => pt.copy(variance = variance.flip)) :+ resultType.copy(variance = variance)
      TypeEntity(name(paramTypes.length), variance, typeArgs)
    }

    def unapply(tpe: TypeEntity) =
      (!tpe.args.isEmpty && tpe.name == name(tpe.args.size - 1)).option((tpe.args.init, tpe.args.last, tpe.variance))
  }

  object MemberAccess {
    val name = "<memberAccess>"

    def apply(owner: TypeEntity, member: TypeEntity): TypeEntity =
      TypeEntity(name, Covariant, owner.copy(variance = Contravariant) :: member :: Nil)

    def unapply(tpe: TypeEntity) =
      (tpe.args.size == 2 && tpe.name == name).option((tpe.args.head, tpe.args.tail.head))
  }

  object Ignored {
    def name(n: Int) = s"<ignored$n>"

    def apply(typeArgs: List[TypeEntity], variance: Variance = Covariant) =
      TypeEntity(name(typeArgs.length), variance, typeArgs)

    def unapply(tpe: TypeEntity) =
      (tpe.name == name(tpe.args.length)).option((tpe.args, tpe.variance))
  }

  def apply(name: String, args: List[TypeEntity] = Nil): TypeEntity =
    TypeEntity(name, Covariant, args)
}

case class TypeParameterEntity(
  name: String,
  variance: Variance,
  lowerBound: String = TypeEntity.Nothing.name,
  upperBound: String = TypeEntity.Any.name) {
  import TypeEntity._

  override def toString() = {
    val lbound =
      if (lowerBound == Nothing.name) ""
      else s" >: $lowerBound"

    val ubound =
      if (upperBound == Any.name) ""
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

package scaps.webapi

import scala.collection.mutable.ListBuffer

trait EntityLike {
  def name: String

  def shortName: String =
    EntityName.splitName(name).last
}

/**
 * Helper methods for de-/encoding entity names.
 *
 * We use a plain string representation for entity names because a more
 * sophisticated representation is too expensive to create during extraction.
 */
object EntityName {
  /**
   * Creates a new name referring to a static member (package/module member)
   * and takes care of the encoding.
   */
  def appendMember(ownerName: String, memberId: String): String =
    if (ownerName == "")
      memberId
    else
      ownerName + '.' + memberId

  /**
   * Splits an encoded name into the decoded identifiers it is composed of.
   * E.g. "pkg.Cls.member" becomes List("pkg", "Cls", "member").
   */
  def splitName(name: String): List[String] = {
    name.split('.').toList
  }
}

sealed trait Definition extends EntityLike

case class TypeDef(
  name: String,
  typeParameters: List[TypeParameterEntity],
  baseTypes: List[TypeRef],
  referencedFrom: Set[Module] = Set(),
  comment: String = "",
  typeFrequency: Map[Variance, Float] = Map())
  extends Definition {

  override def toString() = {
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    val bases = baseTypes.mkString("extends ", " with ", "")

    s"$name$params $bases"
  }

  def isFunction = typeParameters.length > 0 && name == TypeRef.Function.name(typeParameters.length - 1)

  def frequency(v: Variance) =
    typeFrequency.get(v).getOrElse(0f)

  def toType: TypeRef =
    TypeRef(name, Covariant, typeParameters.map(p => TypeRef(p.name, p.variance, Nil, true)))
}

case class ValueDef(
  name: String,
  typeParameters: List[TypeParameterEntity],
  tpe: TypeRef,
  comment: String,
  flags: Set[ValueDef.Flag] = Set(),
  module: Module = Module.Unknown)
  extends Definition {

  override def toString() = {
    val c = comment match {
      case "" => ""
      case _  => s"$comment\n"
    }
    val mods = flags.map(_.name).mkString(" ")
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    s"$c$mods $name$params: $tpe"
  }

  /**
   * A unique description of the value including its name and type.
   */
  def signature: String = {
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    s"$name$params: ${tpe.signature}"
  }

  def typeFingerprint: List[String] =
    tpe.normalize(typeParameters).typeFingerprint

  def withoutComment = copy(comment = "")

  def isOverride = flags(ValueDef.Overrides)

  def isImplicit = flags(ValueDef.Implicit)

  def isStatic = flags(ValueDef.Static)
}

object ValueDef {
  sealed trait Flag {
    def name: String
  }
  case object Overrides extends Flag {
    val name = "overrides"
  }
  case object Implicit extends Flag {
    val name = "implicit"
  }
  case object Static extends Flag {
    val name = "static"
  }
}

case class TypeRef(name: String, variance: Variance, args: List[TypeRef], isTypeParam: Boolean = false) extends EntityLike {
  import TypeRef._

  override def toString() = {
    val argStr = args match {
      case Nil => ""
      case as  => as.mkString("[", ", ", "]")
    }
    s"${variance.prefix}$name$argStr"
  }

  def signature: String =
    this match {
      case Implicit(t, _) =>
        t.signature
      case _ =>
        val argStr = args match {
          case Nil => ""
          case as  => as.map(_.signature).mkString("[", ", ", "]")
        }
        s"$name$argStr"
    }

  def fingerprint: String = s"${variance.prefix}$name"

  def typeFingerprint: List[String] = this.toList
    .filter {
      case TypeRef.Ignored(_, _) => false
      case _                        => true
    }
    .map(_.fingerprint)
    .sorted

  def toList: List[TypeRef] = this :: args.flatMap(_.toList)

  def withVariance(v: Variance): TypeRef =
    if (variance == v) this
    else copy(variance = v, args = args.map(arg => arg.withVariance(arg.variance * v)))

  def transform(f: TypeRef => TypeRef): TypeRef =
    f(this.copy(args = args.map(_.transform(f))))

  def renameTypeParams(getName: String => String): TypeRef =
    transform { tpe =>
      if (tpe.isTypeParam) tpe.copy(name = getName(name))
      else tpe
    }

  def withArgsAsParams: TypeRef =
    copy(args = args.zipWithIndex.map {
      case (arg, idx) =>
        TypeRef(s"$$$idx", arg.variance, Nil, isTypeParam = true)
    })

  def normalize(typeParams: List[TypeParameterEntity] = Nil): TypeRef = {
    def loop(tpe: TypeRef): TypeRef =
      tpe match {
        case MemberAccess(owner, member) =>
          loop(Function(owner :: Nil, loop(member)))
        case MethodInvocation(args, res, _) =>
          loop(Function(args, loop(res)))
        case Function(a :: (as @ (_ :: _)), res, v) =>
          // curry function
          loop(Function(a :: Nil, Function(as, res, v), v))
        case Refinement(args, v) =>
          val newArgs = args.flatMap {
            case AnyRef(_) => None
            case arg       => Some(arg.copy(args = arg.args.map(loop)))
          }

          newArgs match {
            case arg :: Nil => arg
            case args       => Refinement(args, v)
          }
        case ByName(arg, _) =>
          loop(arg)
        case Implicit(arg, _) =>
          loop(arg)
        case tpe: TypeRef =>
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

    def paramsAndReturnTpes(tpe: TypeRef): List[TypeRef] = tpe match {
      case Function(args, res, v) =>
        args ++ paramsAndReturnTpes(res)
      case tpe => List(tpe)
    }

    // ouvalueost function applications are ignored
    Ignored((loop _ andThen paramsAndReturnTpes)(this))
  }

  def withoutImplicitParams: TypeRef = this match {
    case TypeRef.MethodInvocation(a :: as, res, _) if a.name == TypeRef.Implicit.name =>
      res.withoutImplicitParams
    case TypeRef.MethodInvocation(args, res, v) =>
      TypeRef.MethodInvocation(args, res.withoutImplicitParams, v)
    case t =>
      t
  }

  def etaExpanded: TypeRef = this match {
    case TypeRef.MethodInvocation(args, res, v) =>
      TypeRef.Function(args, res.etaExpanded, v)
    case t =>
      t
  }
}

object TypeRef {
  object Any extends PrimitiveType("scala.Any")
  object AnyVal extends PrimitiveType("scala.AnyVal")
  object AnyRef extends PrimitiveType("java.lang.Object")
  object Int extends PrimitiveType("scala.Int")
  object Long extends PrimitiveType("scala.Long")
  object Float extends PrimitiveType("scala.Float")
  object Char extends PrimitiveType("scala.Char")
  object String extends PrimitiveType("java.lang.String")
  object Nothing extends PrimitiveType("scala.Nothing") {
    val cls = TypeDef(name, Nil, Nil)
  }

  object Unknown extends PrimitiveType("<unknown>")

  class PrimitiveType(val name: String) {
    def apply(variance: Variance = Covariant) = TypeRef(name, variance, Nil)

    def unapply(tpe: TypeRef): Option[Variance] =
      if (tpe.name == name && tpe.args.isEmpty)
        Some(tpe.variance)
      else
        None
  }

  object ByName extends GenericType("<byname>")
  object Repeated extends GenericType("<repeated>")
  object Implicit extends GenericType("<implicit>")
  object Option extends GenericType("scala.Option")
  object Seq extends GenericType("scala.collection.Seq")
  object SList extends GenericType("scala.collection.immutable.List")

  class GenericType(val name: String) {
    def apply(arg: TypeRef, variance: Variance = Covariant) =
      TypeRef(name, variance, arg :: Nil)

    def unapply(tpe: TypeRef): Option[(TypeRef, Variance)] =
      if (tpe.name == name && tpe.args.length == 1)
        Some((tpe.args.head, tpe.variance))
      else
        None

    def matches(tpe: TypeRef): Boolean =
      unapply(tpe).isDefined
  }

  object SMap extends GenericType2("scala.collection.immutable.Map")

  class GenericType2(val name: String) {
    def apply(arg1: TypeRef, arg2: TypeRef, variance: Variance = Covariant) =
      TypeRef(name, variance, arg1 :: arg2 :: Nil)

    def unapply(tpe: TypeRef): Option[(TypeRef, TypeRef, Variance)] =
      if (tpe.name == name && tpe.args.length == 2)
        Some((tpe.args(0), tpe.args(1), tpe.variance))
      else
        None

    def matches(tpe: TypeRef): Boolean =
      unapply(tpe).isDefined
  }

  object Tuple extends VariantType("scala.Tuple")
  object Refinement extends VariantType("<refinement", ">")

  class VariantType(val tpePrefix: String, val tpeSuffix: String = "") {
    def name(n: Int) = s"$tpePrefix$n$tpeSuffix"

    def apply(args: List[TypeRef], variance: Variance = Covariant) =
      TypeRef(name(args.size), variance, args.map(pt => pt.copy(variance = variance)))

    def unapply(tpe: TypeRef): Option[(List[TypeRef], Variance)] =
      if (!tpe.args.isEmpty && tpe.name == name(tpe.args.size))
        Some((tpe.args, tpe.variance))
      else
        None
  }

  object Function extends FunctionLikeType("scala.Function")
  object MethodInvocation extends FunctionLikeType("<methodInvocation", ">")

  class FunctionLikeType(val tpePrefix: String, val tpeSuffix: String = "") {
    def name(n: Int) = s"$tpePrefix$n$tpeSuffix"

    def apply(paramTypes: List[TypeRef], resultType: TypeRef, variance: Variance = Covariant) = {
      val typeArgs = paramTypes.map(pt => pt.copy(variance = variance.flip)) :+ resultType.copy(variance = variance)
      TypeRef(name(paramTypes.length), variance, typeArgs)
    }

    def unapply(tpe: TypeRef) =
      if (!tpe.args.isEmpty && tpe.name == name(tpe.args.size - 1))
        Some((tpe.args.init, tpe.args.last, tpe.variance))
      else
        None
  }

  object MemberAccess {
    val name = "<memberAccess>"

    def apply(owner: TypeRef, member: TypeRef): TypeRef =
      TypeRef(name, Covariant, owner.copy(variance = Contravariant) :: member :: Nil)

    def unapply(tpe: TypeRef) =
      if (tpe.args.size == 2 && tpe.name == name)
        Some((tpe.args.head, tpe.args.tail.head))
      else
        None
  }

  object Ignored {
    def name(n: Int) = s"<ignored$n>"

    def apply(typeArgs: List[TypeRef], variance: Variance = Covariant) =
      TypeRef(name(typeArgs.length), variance, typeArgs)

    def unapply(tpe: TypeRef) =
      if (tpe.name == name(tpe.args.length))
        Some((tpe.args, tpe.variance))
      else
        None
  }
}

case class TypeParameterEntity(
  name: String,
  variance: Variance,
  lowerBound: String = TypeRef.Nothing.name,
  upperBound: String = TypeRef.Any.name) {
  import TypeRef._

  override def toString() = {
    val lbound =
      if (lowerBound == Nothing.name) ""
      else s" >: $lowerBound"

    val ubound =
      if (upperBound == Any.name) ""
      else s" <: $upperBound"

    s"$name$lbound$ubound"
  }
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

case class Module(organization: String, name: String, revision: String) {
  def moduleId = s"$organization:$name:$revision"
  def isSnapshot = revision.endsWith("SNAPSHOT")
}

object Module {
  val Unknown = Module("unknown", "unknown", "0.1.0-SNAPSHOT")
}

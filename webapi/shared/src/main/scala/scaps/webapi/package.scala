package scaps.webapi

import scala.collection.mutable.ListBuffer

trait EntityLike {
  def name: String

  def decodedName: String =
    EntityName.decodeFullName(name)

  def shortName: String =
    EntityName.splitName(name).last
}

sealed trait Entity extends EntityLike

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
      ownerName + '.' + encodeIdentifier(memberId)

  /**
   * Splits an encoded name into the decoded identifiers it is composed of.
   * E.g. "pkg.Cls.member" becomes List("pkg", "Cls", "member").
   */
  def splitName(name: String): List[String] = {
    name.split('.').toList
  }

  def decodeFullName(name: String): String = {
    name
  }

  /**
   * Encodes a Scala identifier such that # characters are escaped by single quotes (').
   */
  def encodeIdentifier(id: String): String = {
    id
  }
}

case class ClassEntity(
  name: String,
  typeParameters: List[TypeParameterEntity],
  baseTypes: List[TypeEntity],
  referencedFrom: Set[Module] = Set(),
  comment: String = "",
  typeFrequency: Map[Variance, Float] = Map())
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

  def frequency(v: Variance) =
    typeFrequency.get(v).getOrElse(0f)

  def toType: TypeEntity =
    TypeEntity(name, Covariant, typeParameters.map(p => TypeEntity(p.name, p.variance, Nil, true)))
}

case class TermEntity(
  name: String,
  typeParameters: List[TypeParameterEntity],
  tpe: TypeEntity,
  comment: String,
  flags: Set[TermEntity.Flag] = Set(),
  module: Module = Module.Unknown)
  extends Entity {

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
   * A unique description of the term including its name and type.
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

  def isOverride = flags(TermEntity.Overrides)

  def isImplicit = flags(TermEntity.Implicit)

  def isStatic = flags(TermEntity.Static)
}

object TermEntity {
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

case class TypeEntity(name: String, variance: Variance, args: List[TypeEntity], isTypeParam: Boolean = false) extends EntityLike {
  import TypeEntity._

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
      case TypeEntity.Ignored(_, _) => false
      case _                        => true
    }
    .map(_.fingerprint)
    .sorted

  def toList: List[TypeEntity] = this :: args.flatMap(_.toList)

  def withVariance(v: Variance): TypeEntity =
    if (variance == v) this
    else copy(variance = v, args = args.map(arg => arg.withVariance(arg.variance * v)))

  def transform(f: TypeEntity => TypeEntity): TypeEntity =
    f(this.copy(args = args.map(_.transform(f))))

  def renameTypeParams(getName: String => String): TypeEntity =
    transform { tpe =>
      if (tpe.isTypeParam) tpe.copy(name = getName(name))
      else tpe
    }

  def withArgsAsParams: TypeEntity =
    copy(args = args.zipWithIndex.map {
      case (arg, idx) =>
        TypeEntity(s"$$$idx", arg.variance, Nil, isTypeParam = true)
    })

  def normalize(typeParams: List[TypeParameterEntity] = Nil): TypeEntity = {
    def loop(tpe: TypeEntity): TypeEntity =
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

    def paramsAndReturnTpes(tpe: TypeEntity): List[TypeEntity] = tpe match {
      case Function(args, res, v) =>
        args ++ paramsAndReturnTpes(res)
      case tpe => List(tpe)
    }

    // outermost function applications are ignored
    Ignored((loop _ andThen paramsAndReturnTpes)(this))
  }

  def withoutImplicitParams: TypeEntity = this match {
    case TypeEntity.MethodInvocation(a :: as, res, _) if a.name == TypeEntity.Implicit.name =>
      res.withoutImplicitParams
    case TypeEntity.MethodInvocation(args, res, v) =>
      TypeEntity.MethodInvocation(args, res.withoutImplicitParams, v)
    case t =>
      t
  }

  def etaExpanded: TypeEntity = this match {
    case TypeEntity.MethodInvocation(args, res, v) =>
      TypeEntity.Function(args, res.etaExpanded, v)
    case t =>
      t
  }
}

object TypeEntity {
  object Any extends PrimitiveType("scala.Any")
  object AnyVal extends PrimitiveType("scala.AnyVal")
  object AnyRef extends PrimitiveType("java.lang.Object")
  object Int extends PrimitiveType("scala.Int")
  object Long extends PrimitiveType("scala.Long")
  object Float extends PrimitiveType("scala.Float")
  object Char extends PrimitiveType("scala.Char")
  object String extends PrimitiveType("java.lang.String")
  object Nothing extends PrimitiveType("scala.Nothing") {
    val cls = ClassEntity(name, Nil, Nil)
  }

  object Unknown extends PrimitiveType("<unknown>")

  class PrimitiveType(val name: String) {
    def apply(variance: Variance = Covariant) = TypeEntity(name, variance, Nil)

    def unapply(tpe: TypeEntity): Option[Variance] =
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
    def apply(arg: TypeEntity, variance: Variance = Covariant) =
      TypeEntity(name, variance, arg :: Nil)

    def unapply(tpe: TypeEntity): Option[(TypeEntity, Variance)] =
      if (tpe.name == name && tpe.args.length == 1)
        Some((tpe.args.head, tpe.variance))
      else
        None

    def matches(tpe: TypeEntity): Boolean =
      unapply(tpe).isDefined
  }

  object SMap extends GenericType2("scala.collection.immutable.Map")

  class GenericType2(val name: String) {
    def apply(arg1: TypeEntity, arg2: TypeEntity, variance: Variance = Covariant) =
      TypeEntity(name, variance, arg1 :: arg2 :: Nil)

    def unapply(tpe: TypeEntity): Option[(TypeEntity, TypeEntity, Variance)] =
      if (tpe.name == name && tpe.args.length == 2)
        Some((tpe.args(0), tpe.args(1), tpe.variance))
      else
        None

    def matches(tpe: TypeEntity): Boolean =
      unapply(tpe).isDefined
  }

  object Tuple extends VariantType("scala.Tuple")
  object Refinement extends VariantType("<refinement", ">")

  class VariantType(val tpePrefix: String, val tpeSuffix: String = "") {
    def name(n: Int) = s"$tpePrefix$n$tpeSuffix"

    def apply(args: List[TypeEntity], variance: Variance = Covariant) =
      TypeEntity(name(args.size), variance, args.map(pt => pt.copy(variance = variance)))

    def unapply(tpe: TypeEntity): Option[(List[TypeEntity], Variance)] =
      if (!tpe.args.isEmpty && tpe.name == name(tpe.args.size))
        Some((tpe.args, tpe.variance))
      else
        None
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
      if (!tpe.args.isEmpty && tpe.name == name(tpe.args.size - 1))
        Some((tpe.args.init, tpe.args.last, tpe.variance))
      else
        None
  }

  object MemberAccess {
    val name = "<memberAccess>"

    def apply(owner: TypeEntity, member: TypeEntity): TypeEntity =
      TypeEntity(name, Covariant, owner.copy(variance = Contravariant) :: member :: Nil)

    def unapply(tpe: TypeEntity) =
      if (tpe.args.size == 2 && tpe.name == name)
        Some((tpe.args.head, tpe.args.tail.head))
      else
        None
  }

  object Ignored {
    def name(n: Int) = s"<ignored$n>"

    def apply(typeArgs: List[TypeEntity], variance: Variance = Covariant) =
      TypeEntity(name(typeArgs.length), variance, typeArgs)

    def unapply(tpe: TypeEntity) =
      if (tpe.name == name(tpe.args.length))
        Some((tpe.args, tpe.variance))
      else
        None
  }
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

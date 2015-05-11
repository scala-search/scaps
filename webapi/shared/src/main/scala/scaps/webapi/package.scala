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
   * Creates a new name referring to a class member and takes care of the encoding.
   */
  def appendClassMember(ownerName: String, memberId: String): String =
    append(ownerName, memberId, '#')

  /**
   * Creates a new name referring to a static member (package/module member)
   * and takes care of the encoding.
   */
  def appendStaticMember(ownerName: String, memberId: String): String =
    append(ownerName, memberId, '.')

  private def append(ownerName: String, memberId: String, separator: Char): String =
    if (ownerName == "")
      memberId
    else
      ownerName + separator + encodeIdentifier(memberId)

  /**
   * Splits an encoded name into the decoded identifiers it is composed of.
   * E.g. "pkg.Cls#member" becomes List("pkg", "Cls", "member").
   */
  def splitName(name: String): List[String] = {
    val lb = new ListBuffer[String]
    var currentName = new StringBuilder
    var i = 0
    while (i < name.length()) {
      val c = name(i)
      if (c == ''') {
        i += 1
        currentName.append(name(i))
      } else if (c == '#' || c == '.') {
        lb.append(currentName.mkString)
        currentName = new StringBuilder
      } else {
        currentName.append(c)
      }
      i += 1
    }
    lb.append(currentName.mkString)
    lb.result()
  }

  def decodeFullName(name: String): String = {
    val decoded = new StringBuilder
    var i = 0
    while (i < name.length()) {
      val c = name(i)
      if (c == ''') {
        i += 1
        decoded.append(name(i))
      } else {
        decoded.append(c)
      }
      i += 1
    }
    decoded.mkString
  }

  /**
   * Encodes a Scala identifier such that # characters are escaped by single quotes (').
   */
  def encodeIdentifier(id: String): String = {
    if (id.indexOf('#') == -1) {
      id
    } else {
      val sb = new StringBuilder
      for (c <- id) {
        if (c == '#')
          sb.append("'#")
        else
          sb.append(c)
      }
      sb.mkString
    }
  }
}

case class ClassEntity(
  name: String,
  typeParameters: List[TypeParameterEntity],
  baseTypes: List[TypeEntity],
  referencedFrom: Set[Module] = Set(),
  comment: String = "")
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

case class TermEntity(
  name: String,
  typeParameters: List[TypeParameterEntity],
  tpe: TypeEntity,
  comment: String,
  module: Module = Module.Unknown)
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

case class TypeEntity(name: String, variance: Variance, args: List[TypeEntity]) extends EntityLike {
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

  def toList: List[TypeEntity] = this :: args.flatMap(_.toList)

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
        case ByName(arg, _) =>
          loop(arg)
        case Repeated(arg, _) =>
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
  object AnyVal extends PrimitiveType("scala.AnyVal")
  object AnyRef extends PrimitiveType("java.lang.Object")
  object Int extends PrimitiveType("scala.Int")
  object Char extends PrimitiveType("scala.Char")
  object String extends PrimitiveType("java.lang.String")
  object Nothing extends PrimitiveType("scala.Nothing")

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

  class GenericType(val name: String) {
    def apply(arg: TypeEntity, variance: Variance = Covariant) =
      TypeEntity(name, variance, arg :: Nil)

    def unapply(tpe: TypeEntity): Option[(TypeEntity, Variance)] =
      if (tpe.name == name && tpe.args.length == 1)
        Some((tpe.args.head, tpe.variance))
      else
        None
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

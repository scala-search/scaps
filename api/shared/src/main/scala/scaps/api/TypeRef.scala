package scaps.api

case class TypeRef(name: String, variance: Variance, args: List[TypeRef], isTypeParam: Boolean = false) {
  import TypeRef._

  def shortName: String =
    EntityName.splitName(name).last

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

  def annotatedSignature: String =
    this match {
      case Implicit(t, _) =>
        t.annotatedSignature
      case _ =>
        val argStr = args match {
          case Nil => ""
          case as  => as.map(_.annotatedSignature).mkString("[", ", ", "]")
        }
        s"${variance.prefix}$name$argStr"
    }

  def fingerprint: String = s"${variance.prefix}$name"

  def typeFingerprint: List[String] = this.toList
    .filter {
      case TypeRef.Ignored(_, _) => false
      case _                     => true
    }
    .map(_.fingerprint)
    .sorted

  def toList: List[TypeRef] = this :: args.flatMap(_.toList)

  def withVariance(v: Variance): TypeRef =
    if (variance == v) this
    else if (v == Invariant)
      copy(variance = v, args = args.map(arg => arg.withVariance(arg.variance * v)))
    else
      copy(variance = v, args = args.map(arg => arg.withVariance(arg.variance.flip)))

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

  def normalize(typeParams: List[TypeParameter] = Nil): TypeRef = {
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

    // outermost function applications are ignored
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

  def functionArity: Int = this.normalize(Nil) match {
    case TypeRef.Function(args, r, v) => args.length + r.functionArity
    case t                            => 0
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
    val cls = TypeDef(name, Nil)
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
  object Repeated extends GenericType("scala.<repeated>")
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

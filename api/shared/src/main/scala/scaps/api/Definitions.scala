/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.api

sealed trait Definition {
  def name: String

  def shortName: String =
    EntityName.splitName(name).last

  def module: Module

  def withModule(m: Module): this.type = (this match {
    case t: TypeDef  => t.copy(module = m)
    case v: ValueDef => v.copy(module = m)
    case v: ViewDef  => v.copy(module = m)
  }).asInstanceOf[this.type]
}

case class TypeDef(
  name: String,
  typeParameters: List[TypeParameter],
  comment: String = "",
  module: Module = Module.Unknown,
  typeFrequency: Map[Variance, Float] = Map())
    extends Definition {

  override def toString() = {
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    s"$name$params"
  }

  def isFunction = typeParameters.length > 0 && name == TypeRef.Function.name(typeParameters.length - 1)

  def frequency(v: Variance) =
    typeFrequency.get(v).getOrElse(0f)

  def toType: TypeRef =
    TypeRef(name, Covariant, typeParameters.map(p => TypeRef(p.name, p.variance, Nil, true)))
}

case class ValueDef(
  name: String,
  typeParameters: List[TypeParameter],
  tpe: TypeRef,
  comment: DocComment,
  flags: Set[ValueDef.Flag] = Set(),
  source: Source = UnknownSource,
  module: Module = Module.Unknown,
  docLink: Option[String] = None)
    extends Definition {

  override def toString() = {
    val c = comment match {
      case DocComment.empty => ""
      case _                => s"$comment\n"
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
  def signature: String = signature(false)

  def signature(withImplicits: Boolean): String = {
    val params = typeParameters match {
      case Nil => ""
      case ps  => ps.mkString("[", ", ", "]")
    }
    s"$name$params: ${tpe.signature(withImplicits)}"
  }

  lazy val group = ValueDef(EntityName.splitName(name).last, Nil, tpe.curried.structure, DocComment.empty, Set(), UnknownSource, Module.Unknown, None)

  def withoutComment = copy(comment = DocComment.empty)

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

case class TypeParameter(
    name: String,
    variance: Variance,
    lowerBound: TypeRef = TypeRef.Nothing(Covariant),
    upperBound: TypeRef = TypeRef.Any(Contravariant)) {

  import TypeRef._

  assert(lowerBound.variance == Covariant)
  assert(upperBound.variance == Contravariant)

  override def toString() = {
    val lbound =
      if (lowerBound == Nothing(Covariant)) ""
      else s" >: ${lowerBound.withVariance(Invariant)}"

    val ubound =
      if (upperBound == Any(Contravariant)) ""
      else s" <: ${upperBound.withVariance(Invariant)}"

    s"$name$lbound$ubound"
  }
}

case class ViewDef(from: TypeRef, to: TypeRef, definingEntityName: String = "", module: Module = Module.Unknown)
    extends Definition {
  def name = s"$definingEntityName:$fromKey:$toKey"

  def fromKey = ViewDef.key(from)
  def toKey = ViewDef.key(to)

  def apply(t: TypeRef): Option[TypeRef] = {
    findParamMap(t).map { paramMap =>
      paramMap.foldLeft(to) { (t, paramWithArg) =>
        t(paramWithArg._1, paramWithArg._2)
      }
    }
  }

  def findParamMap(t: TypeRef): Option[List[(String, TypeRef)]] =
    findParamMap(List(from), List(t))

  def findParamMap(from: List[TypeRef], t: List[TypeRef]): Option[List[(String, TypeRef)]] =
    from.zip(t).foldLeft(Option(List[(String, TypeRef)]())) { (paramMapOpt, fromWithT) =>
      paramMapOpt.flatMap { paramOpt =>
        val (f, t) = fromWithT
        if (f.isTypeParam)
          Some((f.name -> t) :: paramOpt)
        else if (f.name == t.name && f.variance == t.variance)
          findParamMap(f.args, t.args).map(_ ::: paramOpt)
        else
          None
      }
    }

  lazy val retainedInformation: Double = {
    if (from.isTypeParam) {
      1d
    } else {
      val fromParts = from.toList
      val toParts = to.toList
      val fromParams = fromParts.filter(_.isTypeParam)
      val droppedParams = fromParams.count(p => !toParts.exists(_.name == p.name))
      (fromParts.size - droppedParams).toDouble / fromParts.size
    }
  }

  def compose(rhs: ViewDef): Option[ViewDef] = {
    rhs(to).map { newTo =>
      ViewDef(from, newTo)
    }
  }
}

object ViewDef {
  def key(tpe: TypeRef) =
    tpe.withArgsAsParams.renameTypeParams(_ => "_").annotatedSignature

  def bidirectional(from: TypeRef, to: TypeRef, definingEntityName: String) =
    List(
      ViewDef(from, to, definingEntityName),
      ViewDef(to.withVariance(to.variance.flip), from.withVariance(from.variance.flip), definingEntityName))
}

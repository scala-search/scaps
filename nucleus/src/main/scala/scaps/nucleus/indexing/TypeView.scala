package scaps.nucleus.indexing

import scaps.nucleus.TypeRef
import scaps.nucleus.IndexSettings
import scaps.nucleus.Definition
import scaps.nucleus.TypeDef
import scaps.nucleus.ValueDef
import scaps.nucleus.TypeParam
import scaps.nucleus.Contravariant
import scaps.nucleus.Covariant
import scaps.nucleus.Invariant
import scaps.nucleus.Type

private[nucleus] case class TypeView(from: TypeRef, to: TypeRef) {

  import scaps.nucleus.indexing.{ InternalTypes => I }

  override def toString =
    s"$from |> $to"

  def apply(t: TypeRef): Option[TypeRef] = {
    if (from.variance == t.variance) {
      val paramMap = findParamMap(from, t)
      Some(paramMap.foldLeft(to) { (t, paramWithArg) =>
        t(paramWithArg._1, paramWithArg._2)
      })
    } else
      None
  }

  def findParamMap(from: TypeRef, t: TypeRef): List[(String, TypeRef)] =
    from match {
      case I.__(_, _) =>
        List((from.name -> t))
      case _ =>
        from.args.zip(t.args).flatMap {
          case (f, t) => findParamMap(f, t)
        }
    }

  lazy val distance: Float =
    if (from == to) 0 else 1

  lazy val retainedInformation: Float = {
    from match {
      case I.__(_, _) => 1f
      case _ =>
        val fromParts = from.toList
        val toParts = to.toList
        val fromParams = fromParts.filter {
          case I.__(_, _) => true
          case _          => false
        }
        val droppedParams = fromParams.count(p => !toParts.exists(_.name == p.name))
        (fromParts.size - droppedParams).toFloat / fromParts.size
    }
  }
}

private[nucleus] object TypeView {

  import scaps.nucleus.indexing.{ InternalTypes => I }

  def typeViews(d: Definition): List[TypeView] = d match {
    case sd: TypeDef  => typeViews(sd)
    case vd: ValueDef => typeViews(vd)
  }

  private[this] def clean(tpe: Type): TypeRef = {
    val paramsWithNewName = tpe.params.map((_, I.__().name))
    val clean = (TypeNormalization.normalize _) andThen
      (t => TypeNormalization.renameTypeParams(paramsWithNewName, t))

    clean(tpe.ref)
  }

  def typeViews(td: TypeDef): List[TypeView] = {
    val sub = clean(td.tpe)

    td.supertypes.flatMap { superType =>
      val supr = clean(Type(td.tpe.params, superType))

      if (supr == +I.Top())
        Nil
      else
        List(
          TypeView(supr, sub),
          TypeView(sub.atPosition(Contravariant), supr.atPosition(Contravariant)))
    }

  }

  def typeViews(valDef: ValueDef): List[TypeView] = {
    if (valDef.isImplicit) {
      clean(valDef.tpe) match {
        case I.Fn(v, _, res) if res == +I.Top() =>
          Nil
        case I.Fn(v, List(arg), res) =>
          List(
            TypeView(res, arg.atPosition(Covariant)),
            TypeView(arg, res.atPosition(Contravariant)))
        case _ => Nil
      }
    } else
      Nil
  }

  def elementaryTypeViews(td: TypeDef): List[TypeView] = {
    val cleaned = clean(td.tpe)
    val cov = cleaned.atPosition(Covariant)
    val conv = cleaned.atPosition(Contravariant)
    val inv = cleaned.atPosition(Invariant)

    elementaryAlternatives(cov).map(TypeView(cov, _)) ++
      elementaryAlternatives(conv).map(TypeView(conv, _)) ++
      elementaryAlternatives(inv).map(TypeView(inv, _))
  }

  def elementaryAlternatives(tr: TypeRef): List[TypeRef] =
    (tr.variance match {
      case Covariant =>
        List(
          tr,
          I.Bottom(Covariant, Nil),
          I.Unknown(Invariant, Nil),
          tr.atPosition(Invariant),
          I.Bottom(Covariant, tr.args))
      case Contravariant =>
        List(
          tr,
          I.Top(Contravariant, Nil),
          I.Unknown(Invariant, Nil),
          tr.atPosition(Invariant),
          I.Top(Contravariant, tr.args))
      case Invariant =>
        List(
          tr,
          I.Unknown(Invariant, Nil),
          I.Unknown(Invariant, tr.args))
    }).distinct
}

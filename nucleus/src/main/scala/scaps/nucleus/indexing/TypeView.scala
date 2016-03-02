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

private[nucleus] case class TypeView(from: TypeRef, to: TypeRef) {

  import scaps.nucleus.indexing.{ InternalTypes => I }

  override def toString =
    s"$from |> $to"

  def apply(t: TypeRef): Option[TypeRef] = {
    val paramMap = findParamMap(from, t)
    Some(paramMap.foldLeft(to) { (t, paramWithArg) =>
      t(paramWithArg._1, paramWithArg._2)
    })
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

  private[this] def clean(typeParams: List[TypeParam], tpe: TypeRef) = {
    val paramsWithNewName = typeParams.map((_, I.__().name))
    val clean = (TypeNormalization.normalize _) andThen
      (t => TypeNormalization.renameTypeParams(paramsWithNewName, t))

    clean(tpe)
  }

  def typeViews(subDef: TypeDef): List[TypeView] = {
    val sub = clean(subDef.tpe.params, subDef.tpe.ref)

    subDef.supertypes.flatMap { superType =>
      val supr = clean(subDef.tpe.params, superType)

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
      clean(valDef.tpe.params, valDef.tpe.ref) match {
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

  def elementaryTypeViews(params: List[TypeParam], tpe: TypeRef): List[TypeView] = {

    val cleaned = clean(params, tpe)
    val cov = cleaned.atPosition(Covariant)
    val conv = cleaned.atPosition(Contravariant)
    val inv = cleaned.atPosition(Invariant)

    val reflective = List(
      TypeView(cov, cov),
      TypeView(conv, conv),
      TypeView(inv, inv))

    val extremal = List(
      TypeView(cov, I.Bottom(Covariant, Nil)),
      TypeView(conv, I.Top(Contravariant, Nil)),
      TypeView(inv, I.Unknown(Invariant, Nil)),
      TypeView(cov, I.Unknown(Invariant, Nil)),
      TypeView(conv, I.Unknown(Invariant, Nil)))

    val dropVariance = List(
      TypeView(cov, inv),
      TypeView(conv, inv))

    // -List[-_] |> -Top[-_], -Map[\_, +_] |> -Top[\_, +_]
    val extremalWithArgs =
      if (tpe.args.isEmpty)
        Nil
      else {
        List(
          TypeView(cov, I.Bottom(Covariant, cov.args)),
          TypeView(conv, I.Bottom(Contravariant, conv.args)),
          TypeView(inv, I.Bottom(Invariant, cov.args)))
      }

    reflective ::: extremal ::: dropVariance ::: extremalWithArgs
  }
}

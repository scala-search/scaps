/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

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

private[nucleus] case class TypeView(params: List[TypeParam], from: TypeRef, to: TypeRef) {

  import scaps.nucleus.indexing.{ InternalTypes => I }

  override def toString =
    s"[${params.mkString(", ")}] $from |> $to"

  def apply(t: TypeRef): Option[TypeRef] =
    findParamMap(from, t).map { paramMap =>
      paramMap.foldLeft(to) { (t, paramWithArg) =>
        t(paramWithArg._1, paramWithArg._2)
      }
    }

  private def findParamMap(from: TypeRef, t: TypeRef): Option[List[(String, TypeRef)]] =
    from match {
      case _ if params.exists(_.name == from.name) =>
        Some(List((from.name -> t)))
      case _ if from.name == t.name && from.variance == t.variance =>
        val argMaps = from.args.zip(t.args).map {
          case (f, t) => findParamMap(f, t)
        }
        argMaps.foldLeft(Option(List[(String, TypeRef)]())) { (acc, argMap) =>
          acc.flatMap { prevMap =>
            argMap.map(prevMap ++ _)
          }
        }
      case _ =>
        None
    }

  lazy val distance: Float =
    if (from.name == to.name && from.variance == to.variance) 0 else 1

  lazy val retainedInformation: Float = {
    val fromParts = from.toList
    val toParts = to.toList
    val fromParams = fromParts.filter { part => params.exists(_.name == part.name) }
    val droppedParams = fromParams.count(p => !toParts.exists(_.name == p.name))
    (fromParts.size - droppedParams).toFloat / fromParts.size
  }
}

private[nucleus] object TypeView {

  import scaps.nucleus.indexing.{ InternalTypes => I }

  def typeViews(d: Definition): List[TypeView] = d match {
    case sd: TypeDef  => typeViews(sd)
    case vd: ValueDef => typeViews(vd)
  }

  private[this] def clean(tpe: Type): TypeRef = {
    val clean = (TypeNormalization.normalize _)

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
          TypeView(td.tpe.params, supr, sub),
          TypeView(td.tpe.params, sub.atPosition(Contravariant), supr.atPosition(Contravariant)))
    }

  }

  def typeViews(valDef: ValueDef): List[TypeView] = {
    if (valDef.isImplicit) {
      clean(valDef.tpe) match {
        case I.Fn(v, _, res) if res == +I.Top() =>
          Nil
        case I.Fn(v, List(arg), res) =>
          List(
            TypeView(valDef.tpe.params, res, arg.atPosition(Covariant)),
            TypeView(valDef.tpe.params, arg, res.atPosition(Contravariant)))
        case _ => Nil
      }
    } else
      Nil
  }

  def elementaryTypeViews(tpe: Type): List[TypeView] = {
    val cleaned = clean(tpe)
    val cov = cleaned.atPosition(Covariant)
    val conv = cleaned.atPosition(Contravariant)
    val inv = cleaned.atPosition(Invariant)

    elementaryAlternatives(cov).map(TypeView(tpe.params, cov, _)) ++
      elementaryAlternatives(conv).map(TypeView(tpe.params, conv, _)) ++
      elementaryAlternatives(inv).map(TypeView(tpe.params, inv, _))
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

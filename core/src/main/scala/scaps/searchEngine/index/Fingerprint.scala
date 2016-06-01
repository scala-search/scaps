/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine.index

import scaps.api.ValueDef
import scaps.api.TypeRef
import scaps.api.Invariant

case class Fingerprint(termsWithIsOpt: List[(String, Boolean)]) extends AnyVal {
  override def toString =
    termStrings.mkString(" ")

  def termStrings: List[String] = {
    def termStr(term: (String, Boolean)) =
      (if (term._2) "?" else "!") + term._1

    termsWithIsOpt.map(termStr)
  }
}

object Fingerprint {
  def apply(v: ValueDef, polarizedTypes: Boolean = true): Fingerprint = {
    val tpe = v.tpe.normalize(v.typeParameters)
    val polTpe =
      if (polarizedTypes) tpe
      else tpe.withVariance(Invariant)
    Fingerprint(terms(polTpe).sortBy(_._1))
  }

  private val optionalTypes = List(
    TypeRef.ByName.unapply _,
    TypeRef.Repeated.unapply _,
    TypeRef.Refinement.unapply _)

  def terms(t: TypeRef, inImplicit: Boolean = false): List[(String, Boolean)] = t match {
    case TypeRef.Ignored(args, _) => args.flatMap(terms(_, inImplicit))
    case TypeRef.Implicit(arg, _) => terms(arg, true)
    case TypeRef(name, v, args, _) =>
      val optional = inImplicit || optionalTypes.exists(_.apply(t).isDefined)
      (v.prefix + name, optional) :: args.flatMap(terms(_, inImplicit))
  }

  def apply(s: String): Fingerprint =
    Fingerprint(termsFromString(s))

  def termsFromString(s: String): List[(String, Boolean)] = {
    var newTerm = true
    var optionalTerm = false
    var currName = new StringBuilder
    var buff = List.newBuilder[(String, Boolean)]
    s.foreach { c =>
      if (newTerm) {
        c match {
          case '!' => optionalTerm = false
          case '?' => optionalTerm = true
        }
        newTerm = false
      } else {
        c match {
          case ' ' =>
            buff += ((currName.result(), optionalTerm))
            currName.clear()
            newTerm = true
          case c =>
            currName += c
        }
      }
    }
    buff += ((currName.result(), optionalTerm))
    buff.result()
  }
}

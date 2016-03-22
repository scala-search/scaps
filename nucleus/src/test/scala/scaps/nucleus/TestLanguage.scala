/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus

import scaps.nucleus.indexing.InternalTypes;
import scala.language.implicitConversions

import java.util.regex.Pattern

object TestLanguage {
  val testModel = LanguageSettings(
    topTypePattern = Pattern.compile("Any"),
    bottomTypePattern = Pattern.compile("Nothing"),
    repeatedType = None,
    functionTypePattern = Pattern.compile("""(Fn[0-9]+)|((<memberAccess|<methodInvocation)[0-9]+>)"""))

  object A extends InternalTypes.ProperType("A")
  object B extends InternalTypes.ProperType("B")

  object T {
    object Any extends InternalTypes.ProperType("Any")
    object Nothing extends InternalTypes.ProperType("Nothing")

    object Char extends InternalTypes.ProperType("Char")
    object Int extends InternalTypes.ProperType("Int")
    object Long extends InternalTypes.ProperType("Long")
    object String extends InternalTypes.ProperType("String")
    object Unit extends InternalTypes.ProperType("Unit")

    object Seq extends InternalTypes.UnaryType("Seq")
    object List extends InternalTypes.UnaryType("List")
    object Array extends InternalTypes.UnaryType("Array")

    object Set extends InternalTypes.UnaryType("Set")
    object BitSet extends InternalTypes.ProperType("BitSet")

    object Fn extends InternalTypes.FunctionLikeType("Fn", "")

    object MemberAccess extends InternalTypes.FunctionLikeType("<memberAccess")
    object MethodInvocation extends InternalTypes.FunctionLikeType("<methodInvocation")
    object Repeated extends InternalTypes.UnaryType("<repeated>")
  }

  def vall(tpe: TypeRef, isImplicit: Boolean = false) =
    ValueDef("", Type(Nil, tpe), isImplicit, "")

  def deff(args: TypeRef*)(res: TypeRef, isImplicit: Boolean = false) =
    ValueDef("", Type(Nil, T.MethodInvocation(Covariant, args.toList, res)), isImplicit, "")

  def extendss(tpe: TypeRef, base: TypeRef*) =
    TypeDef(Type(Nil, tpe), base.toList, "")

  def tp(name: String) =
    TypeParam(name, None, None, None)

  def extendss(params: TypeParam*)(tpe: TypeRef, base: TypeRef*) =
    TypeDef(Type(params.toList, tpe), base.toList, "")
}

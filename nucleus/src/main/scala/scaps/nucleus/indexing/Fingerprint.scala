/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.indexing

import java.util.regex.Pattern

import scaps.nucleus.Contravariant
import scaps.nucleus.Covariant
import scaps.nucleus.Invariant
import scaps.nucleus.LanguageSettings
import scaps.nucleus.TypeParam
import scaps.nucleus.TypeRef
import scaps.nucleus.ValueDef
import scaps.nucleus.Variance

private[nucleus] case class FingerprintTerm(term: String, isOptional: Boolean) {
  def key: String = s"$term"

  override def toString =
    key
}

private[nucleus] object FingerprintTerm {
  def apply(v: Variance, tpeName: String, isOptional: Boolean = false): FingerprintTerm = {
    FingerprintTerm(v.prefix + tpeName, isOptional)
  }

  def apply(term: String): FingerprintTerm = {
    val isOpt = term(0) match {
      case '!' => false
      case '?' => true
    }
    FingerprintTerm(term.drop(1), isOpt)
  }
}

private[nucleus] object Fingerprint {

  def apply(v: ValueDef): List[FingerprintTerm] = {
    val withoutTypeParams = TypeNormalization.substituteTypeParams(v.tpe)
    val normalized = TypeNormalization.normalize(withoutTypeParams)

    val withOutermostFunctionAppIgnored =
      normalized match {
        case InternalTypes.Fn(v, args, res) =>
          InternalTypes.Ignored(v, args :+ res)
        case t => t
      }

    apply(withOutermostFunctionAppIgnored)
  }

  def apply(t: TypeRef): List[FingerprintTerm] =
    t match {
      case InternalTypes.Ignored(_, args) =>
        args.flatMap(apply)
      case TypeRef(v, name, args) =>
        FingerprintTerm(v, name, isOptional = false) :: args.flatMap(apply _)
    }
}

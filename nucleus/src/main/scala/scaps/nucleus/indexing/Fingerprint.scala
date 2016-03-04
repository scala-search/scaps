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

private[nucleus] case class FingerprintTerm(variance: Variance, tpeName: String, isOptional: Boolean) {
  def key: String = s"${variance.prefix}$tpeName"

  override def toString =
    s"${variance.prefix}$tpeName"
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

package scaps.searchEngine.index

import scalaz._
import scaps.searchEngine.Fingerprint
import scaps.webapi._

object TypeFrequencies {
  def apply(findClass: String => Option[ClassEntity], findSubClasses: TypeEntity => Seq[ClassEntity], terms: Seq[TermEntity]) = {
    // use weak hash map to avoid out of memory exceptions
    val findClassCached = Memo.weakHashMapMemo { findClass }
    val findSubClassesCached = Memo.weakHashMapMemo { findSubClasses }

    def typesReferencedFromTerm(term: TermEntity): Seq[(Variance, String)] =
      Fingerprint.queryFingerprint(findClassCached, findSubClassesCached, term).types
        .map(fpt => (fpt.variance, fpt.name))
        .distinct

    terms
      .filterNot(mayBeInheritedFromAnyRef)
      .flatMap(typesReferencedFromTerm)
      .groupBy(identity)
      .mapValues(_.length)
      .withDefaultValue(0)
  }

  /**
   * Extremely simplified heuristic for fast checks whether a term might be inherited from
   * Any, AnyVal or AnyRef. False positives due to terms with an identical name are ok
   * because this is only used to estimate type frequencies.
   */
  def mayBeInheritedFromAnyRef(term: TermEntity): Boolean =
    term.shortName match {
      case "!=" |
        "##" |
        "==" |
        "asInstanceOf" |
        "eq" |
        "equals" |
        "getClass" |
        "hashCode" |
        "isInstanceOf" |
        "ne" |
        "notify" |
        "notifyAll" |
        "synchronized" |
        "toString" |
        "wait" |
        "$asInstanceOf" |
        "$isInstanceOf" =>
        true
      case _ =>
        false
    }
}

package scaps.searchEngine.index

import scalaz._
import scaps.searchEngine.Fingerprint
import scaps.webapi.ClassEntity
import scaps.webapi.TermEntity
import scaps.webapi.TypeEntity

object TypeFrequencies {
  def apply(findClass: String => Option[ClassEntity], findSubClasses: TypeEntity => Seq[ClassEntity], terms: Seq[TermEntity]) = {
    val findClassCached = Memo.weakHashMapMemo { findClass }
    val findSubClassesCached = Memo.weakHashMapMemo { findSubClasses }

    terms
      .flatMap(t => Fingerprint.queryFingerprint(findClassCached, findSubClassesCached, t).types)
      .map(fpt => (fpt.variance, fpt.name))
      .groupBy(identity)
      .mapValues(_.length)
      .withDefaultValue(0)
  }
}

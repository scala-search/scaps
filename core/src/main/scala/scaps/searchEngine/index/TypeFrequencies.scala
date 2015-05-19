package scaps.searchEngine.index

import scalaz._
import scaps.searchEngine.Fingerprint
import scaps.webapi.ClassEntity
import scaps.webapi.TermEntity
import scaps.webapi.TypeEntity

object TypeFrequencies {
  def apply(findBaseTypes: TypeEntity => Seq[TypeEntity], findSubClasses: TypeEntity => Seq[ClassEntity])(terms: Seq[TermEntity]) = {
    val findBaseTypesCached = Memo.weakHashMapMemo { findBaseTypes }
    val findSubClassesCached = Memo.weakHashMapMemo { findSubClasses }

    terms
      .flatMap(t => Fingerprint.queryFingerprint(findBaseTypesCached, findSubClassesCached, t).types)
      .map(fpt => (fpt.variance, fpt.name))
      .groupBy(identity)
      .mapValues(_.length)
      .withDefaultValue(0)
  }
}

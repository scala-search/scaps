package scaps.searchEngine.index

import scalaz._
import scaps.searchEngine.Fingerprint
import scaps.webapi._
import scala.util.Random
import scaps.utils._

object TypeFrequencies {
  val termsSampleSize = 100000

  def apply(findClass: String => Option[ClassEntity], findSubClasses: TypeEntity => Seq[ClassEntity], terms: Seq[TermEntity]) = {
    // use weak hash map to avoid out of memory exceptions
    val findClassCached = Memo.weakHashMapMemo { findClass }
    val findSubClassesCached = Memo.weakHashMapMemo { findSubClasses }

    def typesReferencedFromTerm(term: TermEntity): Seq[(Variance, String)] =
      Fingerprint.queryFingerprint(findClassCached, findSubClassesCached, term).types
        .map(fpt => (fpt.variance, fpt.name))
        .distinct

    terms
      .sample(termsSampleSize)
      .flatMap(typesReferencedFromTerm)
      .groupBy(identity)
      .mapValues(_.length)
      .withDefaultValue(0)
  }
}

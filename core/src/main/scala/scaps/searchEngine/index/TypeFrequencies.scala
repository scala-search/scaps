package scaps.searchEngine.index

import scalaz._
import scaps.searchEngine.QueryFingerprint
import scaps.webapi._
import scala.util.Random
import scaps.utils._

object TypeFrequencies {
  def apply(findClass: String => Option[ClassEntity], findSubClasses: TypeEntity => Seq[ClassEntity], terms: Seq[TermEntity]) = {
    // use weak hash map to avoid out of memory exceptions
    val findClassCached = Memo.weakHashMapMemo { findClass }
    val findSubClassesCached = Memo.weakHashMapMemo { findSubClasses }

    def typesReferencedFromTerm(term: TermEntity): Seq[(Variance, String)] =
      for {
        tpe <- QueryFingerprint(findClassCached, findSubClassesCached, term).types
        alt <- tpe.alternatives
      } yield (tpe.variance, alt.typeName)

    terms
      .flatMap(typesReferencedFromTerm)
      .groupBy(identity)
      .mapValues(_.length)
      .withDefaultValue(0)
  }
}

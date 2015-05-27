package scaps.searchEngine.index

import scalaz._
import scaps.searchEngine.QueryFingerprint
import scaps.webapi._
import scala.util.Random
import scaps.utils._
import scaps.searchEngine.View

object TypeFrequencies {
  def apply(findAlternativesWithDistance: TypeEntity => Seq[(TypeEntity, Int)], terms: Seq[TermEntity]) = {
    // use weak hash map to avoid out of memory exceptions
    val findAlternativesWithDistanceCached = Memo.weakHashMapMemo { findAlternativesWithDistance }

    def typesReferencedFromTerm(term: TermEntity): Seq[(Variance, String)] =
      for {
        tpe <- QueryFingerprint(findAlternativesWithDistanceCached, term).types
        alt <- tpe.alternatives
      } yield (tpe.variance, alt.typeName)

    terms
      .flatMap(typesReferencedFromTerm)
      .groupBy(identity)
      .mapValues(_.length)
      .withDefaultValue(0)
  }
}

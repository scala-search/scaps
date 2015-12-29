package scaps.searchEngine.index

import scalaz.{ Contravariant => _, _ }
import scaps.api._
import scala.util.Random
import scaps.utils._
import scaps.searchEngine.ApiTypeQuery
import scaps.searchEngine.SemanticError

object TypeFrequencies {
  def apply(analyzeValue: ValueDef => SemanticError \/ ApiTypeQuery,
            values: Seq[ValueDef],
            maxSampleSize: Int): Map[(Variance, String), Float] = {
    def typesReferencedFromValue(value: ValueDef): Seq[(Variance, String)] = {
      analyzeValue(value).map { analyzed =>
        analyzed.allTypes
          .map(tpe => (tpe.variance, tpe.typeName))
          .distinct
      }.getOrElse(Nil)
    }

    val sampledValues = values
      .filter(!_.isOverride)
      .sample(maxSampleSize)

    val maxFrequency = sampledValues.length

    sampledValues
      .flatMap(typesReferencedFromValue)
      .groupBy(identity)
      .mapValues(_.length.toFloat / maxFrequency)
      .withDefaultValue(0)
  }
}

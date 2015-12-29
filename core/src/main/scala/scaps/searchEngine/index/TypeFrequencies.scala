package scaps.searchEngine.index

import scalaz.{ Contravariant => _, _ }
import scaps.api._
import scala.util.Random
import scaps.utils._
import scaps.searchEngine.ApiTypeQuery
import scaps.searchEngine.MaximumClauseCountExceededException

object TypeFrequencies {
  def apply(analyzeValue: ValueDef => ApiTypeQuery,
            values: Seq[ValueDef],
            maxSampleSize: Int): Map[(Variance, String), Float] = {
    def typesReferencedFromValue(value: ValueDef): Seq[(Variance, String)] = {
      val typesInValue = try {
        analyzeValue(value).allTypes
      } catch {
        case MaximumClauseCountExceededException =>
          Nil
      }
      typesInValue
        .map(tpe => (tpe.variance, tpe.typeName))
        .distinct
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

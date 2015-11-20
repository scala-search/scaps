package scaps.searchEngine.index

import scalaz.{ Contravariant => _, _ }
import scaps.api._
import scala.util.Random
import scaps.utils._

object TypeFrequencies {
  def apply(findAlternatives: TypeRef => Seq[TypeRef],
            values: Seq[ValueDef],
            maxSampleSize: Int,
            polarizedTypes: Boolean): Map[(Variance, String), Float] = {
    // use weak hash map to avoid out of memory exceptions
    val findAlternativesCached = Memo.weakHashMapMemo { findAlternatives }

    def types(tpe: TypeRef): Seq[TypeRef] =
      (tpe match {
        case TypeRef.Ignored(args, _) =>
          args.flatMap(types)
        case t @ TypeRef(_, _, args, false) =>
          (t +: findAlternativesCached(t)) ++ args.flatMap(types)
        case _ => Nil
      }).map { t =>
        if (polarizedTypes)
          t
        else
          t.withVariance(Invariant)
      }

    def typesReferencedFromValue(value: ValueDef): Seq[(Variance, String)] =
      for {
        tpe <- types(value.tpe.normalize(value.typeParameters)).distinct
      } yield (tpe.variance, tpe.name)

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

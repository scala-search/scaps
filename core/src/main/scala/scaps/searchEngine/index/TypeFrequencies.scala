package scaps.searchEngine.index

import scalaz.{ Contravariant => _, _ }
import scaps.webapi._
import scala.util.Random
import scaps.utils._

object TypeFrequencies {
  def apply(findAlternatives: TypeRef => Seq[TypeRef],
            values: Seq[ValueDef],
            maxSampleSize: Int): Map[(Variance, String), Float] = {
    // use weak hash map to avoid out of memory exceptions
    val findAlternativesCached = Memo.weakHashMapMemo { findAlternatives }

    def types(tpe: TypeRef): Seq[TypeRef] = {
      def rec(tpe: TypeRef): Seq[TypeRef] =
        tpe match {
          case TypeRef.Ignored(args, _) =>
            args.flatMap(rec)
          case t @ TypeRef(_, _, args, false) =>
            (t +: findAlternativesCached(t.withArgsAsParams)) ++ args.flatMap(rec)
          case _ => Nil
        }

      rec(tpe)
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

  def adjustInvariantTopType(tfs: Map[(Variance, String), Float]): Map[(Variance, String), Float] =
    // the common pattern of having an implicit extension class *Ops which is invariant over T for
    // covariant containers leads to an overboost of the methods in the extension class.
    // The reason is, that an invariant Any has a much lower type frequency than -Any and therefore
    // the query expression for ListOps[Any] achieves usually higher scores than the one for List[Any].
    // This method is a dirty fix and rather arbitrarily but helps significantly in achieving better results!
    tfs
      .updated((Invariant, TypeRef.Any.name), tfs((Contravariant, TypeRef.Any.name)))
      .updated((Invariant, TypeRef.Nothing.name), tfs((Covariant, TypeRef.Nothing.name)))
}

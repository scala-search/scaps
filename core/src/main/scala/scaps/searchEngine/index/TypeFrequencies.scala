package scaps.searchEngine.index

import scalaz._
import scaps.webapi._
import scala.util.Random
import scaps.utils._
import scaps.searchEngine.View

object TypeFrequencies {
  def apply(findAlternatives: TypeEntity => Seq[TypeEntity], terms: Seq[TermEntity]) = {
    // use weak hash map to avoid out of memory exceptions
    val findAlternativesCached = Memo.weakHashMapMemo { findAlternatives }

    def types(tpe: TypeEntity): Seq[TypeEntity] = {
      def rec(tpe: TypeEntity): Seq[TypeEntity] =
        tpe match {
          case TypeEntity.Ignored(args, _) =>
            args.flatMap(rec)
          case t @ TypeEntity(_, _, args, false) =>
            (t +: findAlternativesCached(t.withArgsAsParams)) ++ args.flatMap(rec)
          case _ => Nil
        }

      rec(tpe)
    }

    def typesReferencedFromTerm(term: TermEntity): Seq[(Variance, String)] =
      for {
        tpe <- types(term.tpe.normalize(term.typeParameters))
      } yield (tpe.variance, tpe.name)

    terms
      .flatMap(typesReferencedFromTerm)
      .groupBy(identity)
      .mapValues(_.length)
      .withDefaultValue(0)
  }
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine.index

import scalaz.{ Contravariant => _, _ }
import scaps.api._
import scala.util.Random
import scaps.utils._
import scaps.searchEngine.ApiTypeQuery
import scaps.searchEngine.SemanticError

object TypeFrequencies {
  def apply(alternatives: TypeRef => Seq[TypeRef],
            values: Seq[ValueDef],
            maxSampleSize: Int): Map[(Variance, String), Float] = {
    def subtrees(tpe: TypeRef): List[TypeRef] = {
      tpe :: tpe.args.flatMap(subtrees)
    }

    def analyze(v: ValueDef): Seq[(Variance, String)] =
      for {
        tpe <- subtrees(v.tpe.normalize(v.typeParameters))
        alt <- (tpe +: alternatives(tpe)).map { t => (t.variance, t.name) }
      } yield alt

    def typesReferencedFromValue(value: ValueDef): Seq[(Variance, String)] = {
      analyze(value)
        .flatMap {
          case t @ (Invariant, _) =>
            List(t)
          case t @ (v, tpeName) =>
            List((Invariant, tpeName), t)
        }
        .distinct
    }

    val sampledValues = values
      .sample(maxSampleSize)

    val maxFrequency = sampledValues.length

    Map(sampledValues
      .par
      .flatMap(typesReferencedFromValue)
      .groupBy(identity)
      .mapValues(_.length.toFloat / maxFrequency)
      .seq
      .toSeq: _*)
      .withDefaultValue(0)
  }
}

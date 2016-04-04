/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.querying

import scaps.nucleus.QuerySettings
import scaps.nucleus.indexing.FingerprintTerm

private[nucleus] class QueryScorer(settings: QuerySettings, getTypeFrequency: FingerprintTerm => Double) {
  def scoreQuery(query: ExpandedQuery): QueryExpression = query match {
    case ExpandedQuery.Sum(cs) =>
      QueryExpression.Sum(cs.map(scoreQuery))
    case ExpandedQuery.Max(cs) =>
      QueryExpression.Max(cs.map(scoreQuery))
    case l @ ExpandedQuery.Leaf(v, name, _, _, _) =>
      val term = FingerprintTerm(v, name)
      QueryExpression.Leaf(term.key, boost(l).toFloat, getTypeFrequency(term).toFloat)
  }

  private def boost(l: ExpandedQuery.Leaf): Double = {
    l.fraction *
      itf(FingerprintTerm(l.variance, l.name)) *
      math.pow(1 - (0.1 * l.dist), settings.distanceBoostWeight)
  }

  /**
   * The inverse type frequency is defined as log10(10 / (10f + (1 - f)))
   * where f is the type frequency normed by the maximum possible type frequency
   * (see TypeFrequencies).
   */
  private def itf(t: FingerprintTerm): Double = {
    val base = settings.typeFrequencyWeight
    val freq = getTypeFrequency(t)
    math.log(base / (freq * base + (1 - freq) * 1.1)) / math.log(base)
  }
}

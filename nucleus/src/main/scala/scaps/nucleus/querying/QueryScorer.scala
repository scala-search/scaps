package scaps.nucleus.querying

import scaps.nucleus.QuerySettings
import scaps.nucleus.indexing.FingerprintTerm

private[nucleus] class QueryScorer(settings: QuerySettings, getTypeFrequency: FingerprintTerm => Double) {
  def scoreQuery(query: ExpandedQuery): QueryExpression = query match {
    case ExpandedQuery.Sum(cs) =>
      QueryExpression.Sum(cs.map(scoreQuery))
    case ExpandedQuery.Max(cs) =>
      QueryExpression.Max(cs.map(scoreQuery))
    case ExpandedQuery.Leaf(v, name, fraction, depth, dist) =>
      QueryExpression.Leaf(FingerprintTerm(v, name).key, 0, fraction.toFloat)
  }

  private def boost(settings: QuerySettings, l: ExpandedQuery.Leaf): Double = {
    (if (settings.fractions) l.fraction else 1d) *
      itf(FingerprintTerm(l.variance, l.name)) *
      math.pow(l.dist, settings.distanceBoostWeight)
  }

  /**
   * The inverse type frequency is defined as log10(10 / (10f + (1 - f)))
   * where f is the type frequency normed by the maximum possible type frequency
   * (see TypeFrequencies).
   */
  private def itf(t: FingerprintTerm): Double = {
    val base = settings.typeFrequencyWeight
    if (base == 0) {
      1
    } else {
      val freq = getTypeFrequency(t)
      math.log(base / (freq * base + (1 - freq))) / math.log(base)
    }
  }
}

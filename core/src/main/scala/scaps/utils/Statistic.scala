package scaps.utils

object Statistic {
  /**
   * Implements http://en.wikipedia.org/wiki/Weighted_geometric_mean
   */
  def weightedGeometricMean(elemsWithWeight: (Double, Double)*) =
    math.exp(
      elemsWithWeight.map { case (x, w) => w * math.log(x) }.sum / elemsWithWeight.map { case (_, w) => w }.sum)

  def weightedArithmeticMean(elemsWithWeight: (Double, Double)*) =
    elemsWithWeight.map { case (x, w) => x * w }.sum / elemsWithWeight.map { case (_, w) => w }.sum

  /**
   * Implements https://en.wikipedia.org/wiki/Harmonic_mean#Weighted_harmonic_mean
   */
  def weightedHarmonicMean(elemsWithWeight: (Double, Double)*) =
    elemsWithWeight.map(_._2).sum / elemsWithWeight.map { case (x, w) => w / x }.sum
}

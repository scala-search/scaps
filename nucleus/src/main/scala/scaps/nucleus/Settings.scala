/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus

import java.util.regex.Pattern

case class Settings(
  language: LanguageSettings,
  index: IndexSettings,
  query: QuerySettings)

object Settings {
  private[nucleus] def assertDouble(min: Double, max: Double)(value: Double) = {
    assert(value >= min)
    assert(value <= max)
  }

  private[nucleus] val assertPositive = assertDouble(0d, Double.MaxValue)_
}

case class LanguageSettings(
  topTypePattern: Pattern,
  bottomTypePattern: Pattern,

  repeatedType: Option[String],

  functionTypePattern: Pattern)

case class IndexSettings()

object IndexSettings {
  val default = IndexSettings()
}

case class QuerySettings(
    maxClauseCount: Int,
    maxResults: Int,
    views: Boolean,
    fractions: Boolean,
    penaltyWeight: Double,
    depthBoostWeight: Double,
    distanceBoostWeight: Double,
    typeFrequencyWeight: Double,
    docBoost: Double,
    fingerprintFrequencyCutoff: Double,
    explainScores: Boolean) {

  import Settings._

  assertPositive(maxClauseCount)
  assertPositive(maxResults)
  assertPositive(penaltyWeight)
  assertPositive(depthBoostWeight)
  assertPositive(distanceBoostWeight)
  assertPositive(typeFrequencyWeight)
  assertPositive(docBoost)
  assertPositive(fingerprintFrequencyCutoff)
}

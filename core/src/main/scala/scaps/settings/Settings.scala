/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.settings

import java.io.File
import java.util.concurrent.TimeUnit

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationLong

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

case class Settings(
    index: IndexSettings,
    query: QuerySettings) {

  def modQuery(f: QuerySettings => QuerySettings) = this.copy(query = f(query))
  def modIndex(f: IndexSettings => IndexSettings) = this.copy(index = f(index))
}

object Settings {
  def fromApplicationConf =
    Settings(ConfigFactory.load().getConfig("scaps"))

  def apply(conf: Config): Settings =
    Settings(
      IndexSettings(conf.getConfig("index")),
      QuerySettings(conf.getConfig("query")))

  def assertDouble(min: Double, max: Double)(value: Double) = {
    assert(value >= min)
    assert(value <= max)
  }

  private[settings] val assertPositive = assertDouble(0d, Double.MaxValue)_
}

case class IndexSettings(
    indexDir: String,
    timeout: Duration,
    typeFrequenciesSampleSize: Int,
    polarizedTypes: Boolean) {

  val typeDefsDir = new File(indexDir + "/typeDefs")
  val modulesDir = new File(indexDir + "/modules")
  val valuesDir = new File(indexDir + "/values")
  val viewsDir = new File(indexDir + "/views")

  import Settings._
  assertPositive(typeFrequenciesSampleSize)
}

object IndexSettings {
  def apply(conf: Config): IndexSettings =
    IndexSettings(
      conf.getString("index-dir"),
      conf.getDuration("timeout", TimeUnit.MILLISECONDS).millis,
      conf.getInt("type-frequencies-sample-size"),
      conf.getBoolean("polarized-types"))
}

case class QuerySettings(
    maxClauseCount: Int,
    maxResults: Int,
    views: Boolean,
    fractions: Boolean,
    penaltyWeight: Double,
    typeFrequencyWeight: Double,
    distanceWeight: Double,
    docBoost: Double,
    fingerprintFrequencyCutoff: Double,
    explainScores: Boolean) {

  import Settings._

  assertPositive(maxClauseCount)
  assertPositive(maxResults)
  assertPositive(penaltyWeight)
  assertPositive(typeFrequencyWeight)
  assertPositive(distanceWeight)
  assertPositive(docBoost)
  assertPositive(fingerprintFrequencyCutoff)
}

object QuerySettings {
  def apply(conf: Config): QuerySettings =
    QuerySettings(
      conf.getInt("max-clause-count"),
      conf.getInt("max-results"),
      conf.getBoolean(views),
      conf.getBoolean(fractions),
      conf.getDouble(penaltyWeight),
      conf.getDouble(typeFrequencyWeight),
      conf.getDouble(distanceWeight),
      conf.getDouble(docBoost),
      conf.getDouble(fingerprintFrequencyCutoff),
      conf.getBoolean("explain-scores"))

  val views = "views"

  val fractions = "fractions"

  val penaltyWeight = "penalty-weight"

  val typeFrequencyWeight = "type-frequency-weight"

  val distanceWeight = "distance-weight"

  val docBoost = "doc-boost"

  val fingerprintFrequencyCutoff = "fingerprint-frequency-cutoff"
}

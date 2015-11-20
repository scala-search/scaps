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

object QuerySettings {
  def apply(conf: Config): QuerySettings =
    QuerySettings(
      conf.getInt("max-clause-count"),
      conf.getInt("max-results"),
      conf.getBoolean(views),
      conf.getDouble(penaltyWeight),
      conf.getDouble(depthBoostWeight),
      conf.getDouble(distanceBoostWeight),
      conf.getDouble(typeFrequencyWeight),
      conf.getDouble(docBoost),
      conf.getDouble(fingerprintFrequencyCutoff),
      conf.getBoolean("explain-scores"))

  val views = "views"

  val penaltyWeight = "penalty-weight"

  val depthBoostWeight = "depth-boost-weight"

  val distanceBoostWeight = "distance-boost-weight"

  val typeFrequencyWeight = "type-frequency-weight"

  val docBoost = "doc-boost"

  val fingerprintFrequencyCutoff = "fingerprint-frequency-cutoff"
}

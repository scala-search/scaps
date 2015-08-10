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
  typeFrequenciesSampleSize: Int) {

  val classesDir = new File(indexDir + "/classes")
  val modulesDir = new File(indexDir + "/modules")
  val termsDir = new File(indexDir + "/terms")
  val viewsDir = new File(indexDir + "/views")

  import Settings._
  assertPositive(typeFrequenciesSampleSize)
}

object IndexSettings {
  def apply(conf: Config): IndexSettings =
    IndexSettings(
      conf.getString("index-dir"),
      conf.getDuration("timeout", TimeUnit.MILLISECONDS).millis,
      conf.getInt("type-frequencies-sample-size"))
}

case class QuerySettings(
  maxResults: Int,
  views: Boolean,
  fractions: Boolean,
  lengthNormWeight: Double,
  depthBoostWeight: Double,
  distanceBoostWeight: Double,
  typeFrequencyWeight: Double,
  nameBoost: Double,
  docBoost: Double,
  fingerprintFrequencyCutoff: Double) {

  import Settings._

  assertPositive(maxResults)
  assertPositive(lengthNormWeight)
  assertPositive(depthBoostWeight)
  assertPositive(distanceBoostWeight)
  assertPositive(typeFrequencyWeight)
  assertPositive(nameBoost)
  assertPositive(docBoost)
  assertPositive(fingerprintFrequencyCutoff)
}

object QuerySettings {
  def apply(conf: Config): QuerySettings =
    QuerySettings(
      conf.getInt("max-results"),
      conf.getBoolean(views),
      conf.getBoolean(fractions),
      conf.getDouble(lengthNormWeight),
      conf.getDouble(depthBoostWeight),
      conf.getDouble(distanceBoostWeight),
      conf.getDouble(typeFrequencyWeight),
      conf.getDouble(nameBoost),
      conf.getDouble(docBoost),
      conf.getDouble(fingerprintFrequencyCutoff))

  val views = "views"

  val fractions = "fractions"

  val lengthNormWeight = "length-norm-weight"

  val depthBoostWeight = "depth-boost-weight"

  val distanceBoostWeight = "distance-boost-weight"

  val typeFrequencyWeight = "type-frequency-weight"

  val nameBoost = "name-boost"

  val docBoost = "doc-boost"

  val fingerprintFrequencyCutoff = "fingerprint-frequency-cutoff"
}

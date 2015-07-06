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
  query: QuerySettings)

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
  classesDir: File,
  termsDir: File,
  modulesDir: File,
  viewsDir: File,
  timeout: Duration,
  typeFrequenciesSampleSize: Int) {

  import Settings._
  assertPositive(typeFrequenciesSampleSize)
}

object IndexSettings {
  def apply(conf: Config): IndexSettings =
    IndexSettings(
      new File(conf.getString("classes-dir")),
      new File(conf.getString("terms-dir")),
      new File(conf.getString("modules-dir")),
      new File(conf.getString("views-dir")),
      conf.getDuration("timeout", TimeUnit.MILLISECONDS).millis,
      conf.getInt("type-frequencies-sample-size"))
}

case class QuerySettings(
  maxResults: Int,
  lengthNormWeight: Double,
  parameterCountWeight: Double,
  depthBoostWeight: Double,
  distanceBoostWeight: Double,
  typeFrequencyWeight: Double,
  nameBoost: Double,
  docBoost: Double,
  fingerprintFrequencyCutoff: Double) {

  import Settings._

  assertPositive(maxResults)
  assertPositive(lengthNormWeight)
  assertDouble(0, 1)(parameterCountWeight)
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
      conf.getDouble(lengthNormWeight),
      conf.getDouble(parameterCountWeight),
      conf.getDouble(depthBoostWeight),
      conf.getDouble(distanceBoostWeight),
      conf.getDouble(typeFrequencyWeight),
      conf.getDouble(nameBoost),
      conf.getDouble(docBoost),
      conf.getDouble(fingerprintFrequencyCutoff))

  val lengthNormWeight = "length-norm-weight"

  val parameterCountWeight = "parameter-count-weight"

  val depthBoostWeight = "depth-boost-weight"

  val distanceBoostWeight = "distance-boost-weight"

  val typeFrequencyWeight = "type-frequency-weight"

  val nameBoost = "name-boost"

  val docBoost = "doc-boost"

  val fingerprintFrequencyCutoff = "fingerprint-frequency-cutoff"
}

package scaps.settings

import java.io.File

import scala.collection.JavaConversions.asScalaBuffer

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

case class Settings(
  index: IndexSettings,
  query: QuerySettings)

object Settings {
  def fromApplicationConf =
    Settings(ConfigFactory.load().getConfig("scala-api-search"))

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
  lengthNormWeight: Double) {

  import Settings._
  assertPositive(lengthNormWeight)
}

object IndexSettings {
  def apply(conf: Config): IndexSettings =
    IndexSettings(
      new File(conf.getString("classes-dir")),
      new File(conf.getString("terms-dir")),
      new File(conf.getString("modules-dir")),
      conf.getDouble("length-norm-weight"))
}

case class QuerySettings(
  maxResults: Int,
  distanceBoostGradient: Double,
  depthBoostGradient: Double,
  idfWeight: Double,
  nameBoost: Double,
  docBoost: Double) {

  import Settings._

  assertPositive(distanceBoostGradient)
  assertPositive(depthBoostGradient)
  assertPositive(idfWeight)
  assertPositive(nameBoost)
  assertPositive(docBoost)
}

object QuerySettings {
  def apply(conf: Config): QuerySettings =
    QuerySettings(
      conf.getInt("max-results"),
      conf.getDouble("distance-boost-weight"),
      conf.getDouble("depth-boost-weight"),
      conf.getDouble("idf-weight"),
      conf.getDouble("name-boost"),
      conf.getDouble("doc-boost"))
}

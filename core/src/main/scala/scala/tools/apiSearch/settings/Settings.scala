package scala.tools.apiSearch.settings

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

  private[settings] def assertFloat(min: Float, max: Float)(value: Float) = {
    assert(value >= min)
    assert(value <= max)
  }

  private[settings] val assertPositive = assertFloat(0f, Float.MaxValue)_
}

case class IndexSettings(
  classesDir: File,
  termsDir: File,
  lengthNormWeight: Float) {

  import Settings._
  assertPositive(lengthNormWeight)
}

object IndexSettings {
  def apply(conf: Config): IndexSettings =
    IndexSettings(
      new File(conf.getString("classes-dir")),
      new File(conf.getString("terms-dir")),
      conf.getDouble("length-norm-weight").toFloat)
}

case class QuerySettings(
  maxResults: Int,
  distanceBoostWeight: Float,
  depthBoostWeight: Float,
  idfWeight: Float,
  nameBoost: Float,
  docBoost: Float) {

  import Settings._

  assertFloat(0f, 1f)(distanceBoostWeight)
  assertFloat(0f, 1f)(depthBoostWeight)
  assertPositive(idfWeight)
  assertPositive(nameBoost)
  assertPositive(docBoost)
}

object QuerySettings {
  def apply(conf: Config): QuerySettings =
    QuerySettings(
      conf.getInt("max-results"),
      conf.getDouble("distance-boost-weight").toFloat,
      conf.getDouble("depth-boost-weight").toFloat,
      conf.getDouble("idf-weight").toFloat,
      conf.getDouble("name-boost").toFloat,
      conf.getDouble("doc-boost").toFloat)
}

package scala.tools.apiSearch.settings

import java.io.File

import scala.collection.JavaConversions.asScalaBuffer

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

case class Settings(
  index: IndexSettings,
  query: QuerySettings)

object Settings {
  def fromApplicationConf() =
    Settings(ConfigFactory.load().getConfig("scala-api-search"))

  def apply(conf: Config): Settings =
    Settings(
      IndexSettings(conf.getConfig("index")),
      QuerySettings(conf.getConfig("query")))
}

case class IndexSettings(
  classesDir: File,
  termsDir: File)

object IndexSettings {
  def fromApplicationConf() =
    Settings.fromApplicationConf().index

  def apply(conf: Config): IndexSettings =
    IndexSettings(
      new File(conf.getString("classes-dir")),
      new File(conf.getString("terms-dir")))
}

case class QuerySettings(
  distanceBoostGradient: Float,
  depthBoostGradient: Float,
  idfWeight: Float,
  nameBoost: Float,
  docBoost: Float) {
  import QuerySettings._

  assertFloat(0f, 1f)(distanceBoostGradient)
  assertFloat(0f, 1f)(depthBoostGradient)
  assertPositive(idfWeight)
  assertPositive(nameBoost)
  assertPositive(docBoost)
}

object QuerySettings {
  def fromApplicationConf() =
    Settings.fromApplicationConf().query

  def apply(conf: Config): QuerySettings =
    QuerySettings(
      conf.getDouble("distance-boost-gradient").toFloat,
      conf.getDouble("depth-boost-gradient").toFloat,
      conf.getDouble("idf-weight").toFloat,
      conf.getDouble("name-boost").toFloat,
      conf.getDouble("doc-boost").toFloat)

  private def assertFloat(min: Float, max: Float)(value: Float) = {
    assert(value >= min)
    assert(value <= max)
  }

  private val assertPositive = assertFloat(0f, Float.MaxValue)_
}

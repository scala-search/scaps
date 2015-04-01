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
  depthBoostGradient: Float) {

  QuerySettings.assertFloat(0f, 1f)(distanceBoostGradient)
  QuerySettings.assertFloat(0f, 1f)(depthBoostGradient)
}

object QuerySettings {
  def fromApplicationConf() =
    Settings.fromApplicationConf().query

  def apply(conf: Config): QuerySettings =
    QuerySettings(
      conf.getDouble("distance-boost-gradient").toFloat,
      conf.getDouble("depth-boost-gradient").toFloat)

  private def assertFloat(min: Float, max: Float)(value: Double) = {
    assert(value >= min)
    assert(value <= max)
  }
}

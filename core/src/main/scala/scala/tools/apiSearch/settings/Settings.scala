package scala.tools.apiSearch.settings

import com.typesafe.config.Config
import scala.collection.JavaConversions._
import java.io.File
import com.typesafe.config.ConfigFactory
import java.nio.file.Path
import java.nio.file.Paths

case class Settings(
  extractor: ExtractorSettings,
  classIndex: ClassIndexSettings,
  termIndex: TermIndexSettings,
  query: QuerySettings)

object Settings {
  def fromApplicationConf() =
    Settings(ConfigFactory.load().getConfig("scala-api-search"))

  def apply(conf: Config): Settings =
    Settings(
      ExtractorSettings(conf.getConfig("extractor")),
      ClassIndexSettings(conf.getConfig("class-index")),
      TermIndexSettings(conf.getConfig("term-index")),
      QuerySettings(conf.getConfig("query")))
}

case class ExtractorSettings(
  jars: List[File])

object ExtractorSettings {
  def fromApplicationConf() =
    Settings.fromApplicationConf().extractor

  def apply(conf: Config): ExtractorSettings =
    ExtractorSettings(
      conf.getStringList("jars").map(new File(_)).toList)
}

case class ClassIndexSettings(
  path: Path)

object ClassIndexSettings {
  def fromApplicationConf() =
    Settings.fromApplicationConf().classIndex

  def apply(conf: Config): ClassIndexSettings =
    ClassIndexSettings(
      Paths.get(conf.getString("path")))
}

case class TermIndexSettings(
  path: Path)

object TermIndexSettings {
  def fromApplicationConf() =
    Settings.fromApplicationConf().termIndex

  def apply(conf: Config): TermIndexSettings =
    TermIndexSettings(
      Paths.get(conf.getString("path")))
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

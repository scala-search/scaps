package scaps.scala

import scaps.api.Module
import com.typesafe.config.Config
import scala.collection.JavaConversions._
import com.typesafe.config.ConfigFactory

case class ExtractionSettings(controlHost: String, classpath: List[String], modules: List[ModuleSettings])

object ExtractionSettings {
  def fromApplicationConf =
    ExtractionSettings(ConfigFactory.load().getConfig("scaps.extraction"))

  def apply(conf: Config): ExtractionSettings =
    ExtractionSettings(
      conf.getString("control-host"),
      conf.getStringList("classpath").toList,
      conf.getConfigList("modules").map(ModuleSettings.apply).toList)
}

case class ModuleSettings(module: Module, artifactPath: String, docUrl: Option[String])

object ModuleSettings {
  def apply(conf: Config): ModuleSettings =
    ModuleSettings(
      Module(conf.getString("organization"), conf.getString("name"), conf.getString("revision")),
      conf.getString("artifact"),
      conf.getString("doc-url") match {
        case "" => None
        case s  => Some(s)
      })
}

package scaps.scala

import java.util.concurrent.TimeUnit

import scala.collection.JavaConversions.asScalaBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationLong

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

import scaps.api.Module

case class ExtractionSettings(controlHost: String, maxDefinitionsPerRequest: Int, requestTimeout: Duration, classpath: List[String], modules: List[ModuleSettings])

object ExtractionSettings {
  def fromApplicationConf =
    ExtractionSettings(ConfigFactory.load().getConfig("scaps.extraction"))

  def apply(conf: Config): ExtractionSettings =
    ExtractionSettings(
      conf.getString("control-host"),
      conf.getInt("max-definitions-per-request"),
      conf.getDuration("request-timeout", TimeUnit.SECONDS).seconds,
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

package scaps.webservice

import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config

case class WebserviceSettings(
  interface: String,
  port: Int,
  controlInterface: String,
  controlPort: Int)

object WebserviceSettings {
  def fromApplicationConf =
    WebserviceSettings(ConfigFactory.load().getConfig("scaps.webservice"))

  def apply(conf: Config): WebserviceSettings =
    WebserviceSettings(
      conf.getString("interface"),
      conf.getInt("port"),
      conf.getString("control-interface"),
      conf.getInt("control-port"))
}

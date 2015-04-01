package scala.tools.apiSearch.benchmark

import java.io.File
import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config
import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer
import com.typesafe.config.ConfigObject
import java.net.URL

case class ValidationSettings(downloadDir: File, projects: List[ProjectSettings], queries: Map[String, List[String]])

object ValidationSettings {
  def fromApplicationConf() =
    ValidationSettings(ConfigFactory.load().getConfig("scala-api-search.validation"))

  def apply(conf: Config): ValidationSettings =
    ValidationSettings(
      new File(conf.getString("download-dir")),
      conf.getObject("projects").asScala
        .values
        .map(p => ProjectSettings(p.atPath("temp").getConfig("temp")))
        .toList,
      conf.getObject("queries").asScala
        .mapValues(o => o.atPath("temp").getStringList("temp").asScala.toList)
        .toMap)
}

case class ProjectSettings(name: String, url: URL, dependencies: List[URL])

object ProjectSettings {
  def apply(conf: Config): ProjectSettings =
    ProjectSettings(
      conf.getString("name"),
      new URL(conf.getString("url")),
      conf.getStringList("dependencies").asScala.map(new URL(_)).toList)
}

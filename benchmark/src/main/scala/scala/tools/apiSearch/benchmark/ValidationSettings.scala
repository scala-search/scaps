package scala.tools.apiSearch.benchmark

import java.io.File
import java.net.URL

import scala.collection.JavaConverters._

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

case class ValidationSettings(
  downloadDir: File,
  rebuildIndex: Boolean,
  projects: List[ProjectSettings],
  queries: List[(String, Set[String])])

object ValidationSettings {
  def fromApplicationConf =
    ValidationSettings(ConfigFactory.load().getConfig("scala-api-search.validation"))

  def apply(conf: Config): ValidationSettings =
    ValidationSettings(
      new File(conf.getString("download-dir")),
      conf.getBoolean("rebuild-index"),
      conf.getObject("projects").asScala
        .values
        .map(p => ProjectSettings(p.atPath("temp").getConfig("temp")))
        .toList,
      conf.getObject("queries").asScala
        .map { case (key, o) => (key, o.atPath("temp").getStringList("temp").asScala.toSet) }
        .toList)
}

case class ProjectSettings(url: URL, dependencies: List[DependencySettings]) {
  val name = url.getPath().split("/").last
}

object ProjectSettings {
  def apply(conf: Config): ProjectSettings =
    ProjectSettings(
      new URL(conf.getString("url")),
      conf.getStringList("dependencies").asScala.map(url => DependencySettings(new URL(url))).toList)
}

case class DependencySettings(url: URL) {
  val name = url.getPath().split("/").last
}

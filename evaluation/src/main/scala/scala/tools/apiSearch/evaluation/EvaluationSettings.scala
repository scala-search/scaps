package scala.tools.apiSearch.evaluation

import java.io.File
import java.net.URL
import scala.collection.JavaConverters._
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

case class EvaluationSettings(
  downloadDir: File,
  rebuildIndex: Boolean,
  projects: List[ProjectSettings],
  queries: List[(String, Set[String])])

object EvaluationSettings {
  def fromApplicationConf =
    EvaluationSettings(ConfigFactory.load().getConfig("scala-api-search.evaluation"))

  def apply(conf: Config): EvaluationSettings =
    EvaluationSettings(
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

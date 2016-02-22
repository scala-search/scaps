/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.evaluation

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
    EvaluationSettings(ConfigFactory.load().getConfig("scaps.evaluation"))

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

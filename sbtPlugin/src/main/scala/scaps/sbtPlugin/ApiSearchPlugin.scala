/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.sbtPlugin

import sbt.{ url => sbtUrl, _ }
import sbt.Keys._
import sbt.APIMappings
import scaps.buildInfo.BuildInfo

case class IndexJob(organization: String, name: String, revision: String, artifactPath: String, docUrlPrefix: Option[String])

object ApiSearchPlugin extends AutoPlugin {
  override def trigger = allRequirements

  val scapsArtifact = "scaps-scala"

  object autoImport {
    lazy val indexDependencies = SettingKey[Seq[ModuleID]]("indexDependencies", "Dependencies that will be indexed.")
    lazy val scapsControlHost = SettingKey[String]("scapsControlHost", "Hostname of the Scala API Search control service.")
    lazy val scapsModules = TaskKey[Seq[IndexJob]]("scapsModules", "Modules that will be indexed.")

    lazy val Scaps = config("scaps").extend(Runtime)
  }

  import autoImport._

  lazy val scapsSettings = Seq(
    indexDependencies := Seq(),
    scapsControlHost := "localhost:8081",
    scapsModules := {
      val modules = updateClassifiers.value.configuration(Compile.name).get.modules
      val mappings = apiMappings.in(Compile, doc).value
      val deps = indexDependencies.value.map(d =>
        CrossVersion(scalaVersion.value, scalaBinaryVersion.value)(d))

      modules
        .filter(m => deps.exists(
          d => m.module.organization == d.organization && m.module.name == d.name && m.module.revision == d.revision))
        .flatMap { m =>
          val module = m.module
          val as = m.artifacts
          val mapping = as.flatMap { case (_, f) => mappings.get(f) }.headOption
          val sourceFile = as.collectFirst {
            case (Artifact(name, _, _, Some(Artifact.SourceClassifier), _, _, _), file) => file
          }

          sourceFile.map(f => IndexJob(
            module.organization, module.name, module.revision,
            f.getAbsolutePath(),
            mapping.map(_.toString + "#")))
        }.distinct
    },
    javaOptions := {
      val classpath = (fullClasspath in Compile).value.map {
        case Attributed(f) => f.getAbsolutePath
      }

      val hostArg = s"-Dscaps.extraction.control-host=${scapsControlHost.value}"
      val cpArgs = classpath.zipWithIndex.map {
        case (cp, idx) => s"-Dscaps.extraction.classpath.${idx}=${cp}"
      }

      val moduleArgs = scapsModules.value.zipWithIndex.flatMap {
        case (m, idx) =>
          Seq(
            s"-Dscaps.extraction.modules.${idx}.organization=${m.organization}",
            s"-Dscaps.extraction.modules.${idx}.name=${m.name}",
            s"-Dscaps.extraction.modules.${idx}.revision=${m.revision}",
            s"-Dscaps.extraction.modules.${idx}.artifact=${m.artifactPath}",
            s"-Dscaps.extraction.modules.${idx}.doc-url=${m.docUrlPrefix.getOrElse("")}")
      }

      (hostArg +: (cpArgs ++ moduleArgs))
    },
    fork := true,
    mainClass := Some("scaps.scala.Main"))

  override lazy val projectSettings = inConfig(Scaps)(Defaults.compileSettings ++ scapsSettings) ++
    Seq(libraryDependencies += BuildInfo.organization %% scapsArtifact % BuildInfo.version)
}

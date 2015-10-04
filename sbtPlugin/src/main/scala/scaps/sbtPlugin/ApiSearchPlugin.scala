package scaps.sbtPlugin

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import autowire._
import sbt.{ url => sbtUrl, _ }
import sbt.Keys._
import sbt.complete.DefaultParsers.spaceDelimited
import sbt.APIMappings
import scaps.api.Module
import scaps.api.ScapsApi
import scaps.api.ScapsControlApi
import org.slf4j.impl.StaticLoggerBinder
import sbt.complete.Parser
import scaps.api.IndexJob
import scaps.api.BuildInfo

object ApiSearchPlugin extends AutoPlugin {
  override def trigger = allRequirements

  val scapsArtifact = "scaps-scala"

  object autoImport {
    lazy val scapsControlHost = SettingKey[String]("scapsControlHost", "Hostname of the Scala API Search control service.")
    lazy val scapsModules = TaskKey[Seq[IndexJob]]("scapsModules", "Modules that will be indexed.")

    lazy val Scaps = config("scaps").extend(Compile)
  }

  import autoImport._

  lazy val scapsSettings = Seq(
    scapsControlHost := "localhost:8081",
    scapsModules := {
      val deps = libraryDependencies.value.collect {
        case ModuleID(_, name, _, _, _, _, _, _, _, _, _) if name != scapsArtifact => name
      }

      val modules = updateClassifiers.value.configuration(Compile.name).get.modules
      val mappings = apiMappings.in(Compile, doc).value

      modules.flatMap { m =>
        val module = m.module
        val as = m.artifacts
        val mapping = as.flatMap { case (_, f) => mappings.get(f) }.headOption
        val sourceFile = as.collectFirst {
          case (Artifact(name, _, _, Some(Artifact.SourceClassifier), _, _, _), file) if deps.exists(name.startsWith(_)) => file
        }

        sourceFile.map(f => IndexJob(
          Module(module.organization, module.name, module.revision),
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
            s"-Dscaps.extraction.modules.${idx}.organization=${m.module.organization}",
            s"-Dscaps.extraction.modules.${idx}.name=${m.module.name}",
            s"-Dscaps.extraction.modules.${idx}.revision=${m.module.revision}",
            s"-Dscaps.extraction.modules.${idx}.artifact=${m.artifactPath}",
            s"-Dscaps.extraction.modules.${idx}.doc-url=${m.docUrlPrefix.getOrElse("")}")
      }

      (hostArg +: (cpArgs ++ moduleArgs))
    },
    fork := true,
    mainClass := Some("scaps.scala.Main"))

  override lazy val projectSettings = inConfig(Scaps)(Defaults.compileSettings ++ scapsSettings) ++ Seq(
    libraryDependencies += BuildInfo.organization %% scapsArtifact % BuildInfo.version)
}

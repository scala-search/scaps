package scala.tools.apiSearch.sbtPlugin

import sbt._
import sbt.Keys._

object ApiSearchPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    lazy val apiSearchIndex = TaskKey[Unit]("apiSearchIndex", "")
  }

  import autoImport._

  override lazy val projectSettings = Seq(
    apiSearchIndex := {
      val logger = streams.value.log

      val deps = libraryDependencies.value.map(_.name)

      val report = updateClassifiers.value
      val sourceArtifactsWithFile = report.configuration(Compile.name).get.modules.flatMap(_.artifacts).filter {
        case (Artifact(name, _, _, Some(Artifact.SourceClassifier), _, _, _), file) if deps.contains(name) =>
          true
        case _ =>
          false
      }
      sourceArtifactsWithFile.map { case (a, f) => (a, f.toString()) }.distinct.foreach {
        case (a, f) =>
          logger.info(a.toString())
          logger.info(f)
      }
      logger.info("")
      ()
    })
}

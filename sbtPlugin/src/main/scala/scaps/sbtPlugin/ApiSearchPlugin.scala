package scaps.sbtPlugin

import sbt.{ url => sbtUrl, _ }
import sbt.Keys._
import sbt.complete.DefaultParsers.spaceDelimited
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scaps.webapi.ScapsApi
import scaps.webapi.ScapsControlApi
import autowire._
import scala.concurrent.Future
import scaps.webapi.Module

object ApiSearchPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    lazy val apiSearchHost = SettingKey[String]("apiSearchHost", "Hostname of the Scala API Search service.")
    lazy val apiSearchControlHost = SettingKey[String]("apiSearchControlHost", "Hostname of the Scala API Search control service.")
    lazy val apiSearchIndex = TaskKey[Unit]("apiSearchIndex", "Requests indexing this project.")
    lazy val api = InputKey[Unit]("api", "Use Scaps to search for terms and functions in the indexed libraries.")
  }

  import autoImport._

  override lazy val projectSettings = Seq(
    apiSearchHost := "localhost:8080",
    apiSearchControlHost := "localhost:8081",
    apiSearchIndex := {
      val logger = streams.value.log
      val scapsControl = controlClient(apiSearchControlHost.value)

      val deps = libraryDependencies.value.map(_.name)

      val modules = updateClassifiers.value.configuration(Compile.name).get.modules

      val sourceFiles = modules.flatMap(m => m.artifacts.map(a => (m.module, a._1, a._2))).filter {
        case (_, Artifact(name, _, _, Some(Artifact.SourceClassifier), _, _, _), file) if deps.contains(name) =>
          true
        case _ =>
          false
      }.map { case (m, a, f) => (Module(m.organization, m.name, m.revision), f.getAbsolutePath()) }.distinct

      val classpath = (fullClasspath in Compile).value.map {
        case Attributed(f) => f.getAbsolutePath
      }

      val f = for {
        _ <- Future.sequence(sourceFiles.map { case (module, file) => scapsControl.index(module, file, classpath).call() })
        status <- scapsControl.getStatus().call()
      } yield {
        logger.info(s"Scaps: ${status.workQueue.size} documents in work queue")
      }

      Await.result(f, 5.seconds)
    },
    api := {
      val logger = streams.value.log
      val query = spaceDelimited("<query>").parsed

      def writeln(s: String) = {
        println(s"[apiSearch] $s")
      }

      val scaps = scapsClient(apiSearchHost.value)

      val msgs = Await.result(scaps.search(query.mkString(" ")).call().map {
        case Left(error) =>
          error :: Nil
        case Right(results) =>
          results.take(3).map(_.signature)
      }, 5.seconds)

      msgs.foreach(writeln)

      ()
    })

  def scapsClient(host: String) =
    new DispatchClient(host)[ScapsApi]

  def controlClient(host: String) =
    new DispatchClient(host)[ScapsControlApi]
}

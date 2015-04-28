package scaps.sbtPlugin

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import autowire._
import sbt.{ url => sbtUrl, _ }
import sbt.Keys._
import sbt.complete.DefaultParsers.spaceDelimited
import scaps.webapi.Module
import scaps.webapi.ScapsApi
import scaps.webapi.ScapsControlApi
import org.slf4j.impl.StaticLoggerBinder

object ApiSearchPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    lazy val scapsHost = SettingKey[String]("scapsHost", "Hostname of the Scala API Search service.")
    lazy val scapsControlHost = SettingKey[String]("scapsControlHost", "Hostname of the Scala API Search control service.")

    lazy val scaps = InputKey[Unit]("scaps", "Use Scaps to search for terms and functions in the indexed libraries.")

    lazy val scapsStatus = TaskKey[Unit]("scapsStatus", "Displays information about the current index state.")
    lazy val scapsModules = TaskKey[Seq[(Module, String)]]("scapsModules", "Modules that will be indexed.")
    lazy val scapsIndex = TaskKey[Unit]("scapsIndex", "Requests indexing this project.")
  }

  import autoImport._

  override lazy val projectSettings = Seq(
    scapsHost := "localhost:8080",
    scapsControlHost := "localhost:8081",
    scaps := {
      val query = spaceDelimited("<query>").parsed

      val scaps = scapsClient(scapsHost.value, streams.value.log)

      def writeln(s: String) = {
        println(s"[apiSearch] $s")
      }

      val msgs = Await.result(scaps.search(query.mkString(" ")).call().map {
        case Left(error) =>
          error :: Nil
        case Right(results) =>
          results.take(3).map(_.signature)
      }, 5.seconds)

      msgs.foreach(writeln)

      ()
    },
    scapsStatus := {
      val log = streams.value.log
      val scaps = scapsClient(scapsHost.value, log)

      for { status <- scaps.getStatus().call() } {
        log.info(s"Scaps Work Queue:")
        for { module <- status.workQueue } {
          log.info(s"  ${module.moduleId}")
        }
      }
    },
    scapsModules := {
      val deps = libraryDependencies.value.collect {
        case ModuleID(_, name, _, _, _, _, _, _, _, _, _) => name
      }

      val modules = updateClassifiers.value.configuration(Compile.name).get.modules

      modules.flatMap(m => m.artifacts.map(a => (m.module, a._1, a._2))).filter {
        case (_, Artifact(name, _, _, Some(Artifact.SourceClassifier), _, _, _), file) if deps.exists(name.startsWith(_)) =>
          true
        case _ =>
          false
      }.map { case (m, a, f) => (Module(m.organization, m.name, m.revision), f.getAbsolutePath()) }.distinct
    },
    scapsIndex := {
      val scapsControl = controlClient(scapsControlHost.value, streams.value.log)

      val classpath = (fullClasspath in Compile).value.map {
        case Attributed(f) => f.getAbsolutePath
      }

      val f = Future.sequence(
        scapsModules.value.map { case (module, file) => scapsControl.index(module, file, classpath).call() })

      Await.result(f, 5.seconds)

      scapsStatus.result
      ()
    })

  def scapsClient(host: String, log: sbt.Logger) = {
    StaticLoggerBinder.sbtLogger = log
    new DispatchClient(host, ScapsApi.apiPath)[ScapsApi]
  }

  def controlClient(host: String, log: sbt.Logger) = {
    StaticLoggerBinder.sbtLogger = log
    new DispatchClient(host, ScapsControlApi.apiPath)[ScapsControlApi]
  }
}

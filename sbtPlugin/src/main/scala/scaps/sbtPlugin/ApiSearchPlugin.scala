package scaps.sbtPlugin

import sbt.{ url => sbtUrl, _ }
import sbt.Keys._
import sbt.complete.DefaultParsers.spaceDelimited
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scaps.webapi.ScapsApi
import autowire._
import scala.concurrent.Future

object ApiSearchPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    lazy val apiSearchHost = SettingKey[String]("apiSearchHost", "Hostname of the Scala API Search service.")
    lazy val apiSearchIndex = TaskKey[Unit]("apiSearchIndex", "Requests indexing this project.")
    lazy val api = InputKey[Unit]("api")
  }

  import autoImport._

  override lazy val projectSettings = Seq(
    apiSearchHost := "localhost:8080",
    apiSearchIndex := {
      val logger = streams.value.log
      val scaps = new DispatchClient(apiSearchHost.value)[ScapsApi]

      val deps = libraryDependencies.value.map(_.name)

      val modules = updateClassifiers.value.configuration(Compile.name).get.modules

      val sourceFiles = modules.flatMap(_.artifacts).filter {
        case (Artifact(name, _, _, Some(Artifact.SourceClassifier), _, _, _), file) if deps.contains(name) =>
          true
        case _ =>
          false
      }.map { case (a, f) => f.getAbsolutePath() }.distinct

      val classpath = (fullClasspath in Compile).value.map {
        case Attributed(f) => f.getAbsolutePath
      }

      val f = for {
        _ <- Future.sequence(sourceFiles.map { sourceFile => scaps.index(sourceFile, classpath).call() })
        status <- scaps.getStatus().call()
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

      val scaps = new DispatchClient(apiSearchHost.value)[ScapsApi]

      val msgs = Await.result(scaps.search(query.mkString(" ")).call().map {
        case Left(error) =>
          error :: Nil
        case Right(results) =>
          results.take(3).map(_.signature)
      }, 5.seconds)

      msgs.foreach(writeln)

      ()
    })
}

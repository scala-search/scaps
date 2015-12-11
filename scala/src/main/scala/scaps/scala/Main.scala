package scaps.scala

import java.io.File
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import autowire._
import scalaz.std.list.listInstance
import scaps.api.ScapsControlApi
import scaps.scala.featureExtraction.JarExtractor
import scaps.scala.featureExtraction.CompilerUtils
import scaps.scala.featureExtraction.ExtractionError
import com.typesafe.scalalogging.StrictLogging
import scaps.api.ValueDef

object Main extends App with StrictLogging {
  val settings = ExtractionSettings.fromApplicationConf

  println(settings)

  val extractor = new JarExtractor(
    CompilerUtils.createCompiler(settings.classpath))

  val scaps = new DispatchClient(settings.controlHost, ScapsControlApi.apiPath)[ScapsControlApi]

  val indexName = Random.nextInt().toString()

  val defs = settings.modules.flatMap { m =>
    ExtractionError.logErrors(extractor(new File(m.artifactPath)), logger.info(_))
      .distinct
      .map(_.withModule(m.module))
      .map {
        case v: ValueDef =>
          val link = for { url <- m.docUrl; suffix <- v.docLink } yield url + suffix
          v.copy(docLink = link)
        case d => d
      }
  }

  defs.grouped(settings.maxDefinitionsPerRequest).foreach { ds =>
    Await.ready(scaps.index(indexName, ds).call(), settings.requestTimeout)
  }

  Await.ready(scaps.finalizeIndex(indexName).call(), settings.requestTimeout)

  System.exit(0)
}

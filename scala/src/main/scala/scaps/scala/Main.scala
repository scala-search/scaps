/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.scala

import java.io.File
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import autowire._
import scalaz.std.list.listInstance
import scalaz.std.stream.streamInstance
import scaps.api.ScapsControlApi
import scaps.scala.featureExtraction.JarExtractor
import scaps.scala.featureExtraction.CompilerUtils
import scaps.scala.featureExtraction.ExtractionError
import com.typesafe.scalalogging.StrictLogging
import scaps.api.ValueDef
import scala.concurrent.Future

object Main extends App with StrictLogging {
  val settings = ExtractionSettings.fromApplicationConf

  println(settings)

  val extractor = new JarExtractor(
    CompilerUtils.createCompiler(settings.classpath))

  val scaps = new DispatchClient(settings.controlHost, ScapsControlApi.apiPath)[ScapsControlApi]

  val indexName = Random.nextInt().toString()

  def defs = settings.modules.toStream.flatMap { m =>
    ExtractionError.logErrors(extractor(new File(m.artifactPath)), logger.info(_))
      .map(_.withModule(m.module))
      .map {
        case v: ValueDef =>
          val link = for { url <- m.docUrl; suffix <- v.docLink } yield url + suffix
          v.copy(docLink = link)
        case d => d
      }
  }

  def indexRequests = defs.grouped(settings.maxDefinitionsPerRequest).map { ds =>
    scaps.index(indexName, ds).call()
  }

  val allIndexRequests = Future.sequence(indexRequests)

  Await.ready(allIndexRequests, settings.requestTimeout)

  Await.ready(scaps.finalizeIndex(indexName).call(), settings.requestTimeout)

  System.exit(0)
}

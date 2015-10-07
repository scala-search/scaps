package scaps.cli

import java.io.File
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scaps.searchEngine.SearchEngine
import scaps.scala.featureExtraction.ExtractionError
import scaps.scala.featureExtraction.JarExtractor
import scaps.settings.Settings
import scaps.api.Module
import scaps.utils.Logging
import scalaz.std.list._
import scaps.scala.featureExtraction.CompilerUtils

object Index extends App with Logging {
  val sourceJar = new File(args(0))

  val settings = Settings.fromApplicationConf

  val compiler = CompilerUtils.createCompiler(Nil)
  val extractor = new JarExtractor(compiler)

  val engine = (SearchEngine(settings)).get

  engine.resetIndexes().get

  val entities =
    ExtractionError.logErrors(extractor(sourceJar), logger.info(_))

  engine.index(entities).get
}

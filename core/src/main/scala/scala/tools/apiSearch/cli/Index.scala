package scala.tools.apiSearch.cli

import java.io.File

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.tools.apiSearch.SearchEngine
import scala.tools.apiSearch.featureExtraction.CompilerUtils
import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.tools.apiSearch.settings.Settings

object Index extends App {
  val sourceJar = new File(args(0))

  val extractor = new JarExtractor(CompilerUtils.initCompiler())

  val engine = new SearchEngine(Settings.fromApplicationConf)

  Await.result(engine.indexEntities(extractor(sourceJar)), 1.hour)
}

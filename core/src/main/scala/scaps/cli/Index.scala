package scaps.cli

import java.io.File
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scaps.searchEngine.SearchEngine
import scaps.featureExtraction.CompilerUtils
import scaps.featureExtraction.JarExtractor
import scaps.settings.Settings
import scaps.webapi.Module

object Index extends App {
  val sourceJar = new File(args(0))

  CompilerUtils.withCompiler() { compiler =>
    val extractor = new JarExtractor(compiler)

    val engine = SearchEngine(Settings.fromApplicationConf).get

    Await.result(engine.indexEntities(Module.Unknown, extractor(sourceJar)), 1.hour)
  }
}

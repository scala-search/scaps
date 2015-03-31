package scala.tools.apiSearch.cli

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.tools.apiSearch.featureExtraction.StandaloneExtractor
import scala.tools.apiSearch.index.Indexer
import scala.tools.apiSearch.settings.Settings

object CreateIndexFromJar extends App {
  val settings = Settings.fromApplicationConf()

  val indexer = new Indexer(settings.index)

  indexer.reset().get

  val entities = StandaloneExtractor(settings.extractor)

  Await.result(indexer.index(entities), 1.hour)
}

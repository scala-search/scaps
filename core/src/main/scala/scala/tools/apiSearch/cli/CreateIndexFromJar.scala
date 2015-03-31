package scala.tools.apiSearch.cli

import scala.tools.apiSearch.featureExtraction.StandaloneExtractor
import scala.tools.apiSearch.utils.CompilerAccess
import org.apache.lucene.store.Directory
import org.apache.lucene.store.FSDirectory
import java.nio.file.Path
import java.io.File
import java.nio.file.Paths
import scala.tools.apiSearch.index.TermsIndex
import scala.tools.apiSearch.index.ClassIndex
import scala.tools.apiSearch.model._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.tools.apiSearch.index.Indexer
import scala.tools.apiSearch.settings.Settings

object CreateIndexFromJar extends App with CompilerAccess {
  val settings = Settings.fromApplicationConf()

  val extractor = new StandaloneExtractor(settings.extractor)

  val indexer = new Indexer(settings.index)

  indexer.reset().get

  val entities = extractor()

  Await.result(indexer.index(entities), 1.hour)
}

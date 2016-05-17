/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine

import java.io.File
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try
import org.apache.lucene.store.NIOFSDirectory
import org.apache.lucene.store.RAMDirectory
import scalaz._
import scalaz.{ \/ => \/ }
import scaps.searchEngine.index.TypeIndex
import scaps.searchEngine.index.ModuleIndex
import scaps.searchEngine.index.ValueIndex
import scaps.searchEngine.index.TypeFrequencies
import scaps.searchEngine.index.ViewIndex
import scaps.searchEngine.queries.QueryAnalyzer
import scaps.searchEngine.queries.QueryParser
import scaps.searchEngine.queries.RawQuery
import scaps.settings.Settings
import scaps.utils.Logging
import scaps.api.TypeDef
import scaps.api.Contravariant
import scaps.api.Covariant
import scaps.api.Definition
import scaps.api.Invariant
import scaps.api.Module
import scaps.api.ValueDef
import scaps.api.TypeRef
import scaps.api.TypeParameter
import scaps.api.Variance
import scaps.api.ViewDef
import scaps.api.Result
import java.util.regex.Pattern
import scaps.api.FingerprintTerm

object SearchEngine {
  def apply(settings: Settings): Try[SearchEngine] = Try {
    def createDir(path: File) = {
      path.mkdirs()
      new NIOFSDirectory(path)
    }

    val values = new ValueIndex(createDir(settings.index.valuesDir), settings)
    val typeDefs = new TypeIndex(createDir(settings.index.typeDefsDir), settings)
    val modules = new ModuleIndex(createDir(settings.index.modulesDir))
    val views = new ViewIndex(createDir(settings.index.viewsDir))

    new SearchEngine(settings, values, typeDefs, modules, views)
  }

  def inMemory(settings: Settings): SearchEngine = {
    val values = new ValueIndex(new RAMDirectory, settings)
    val typeDefs = new TypeIndex(new RAMDirectory, settings)
    val modules = new ModuleIndex(new RAMDirectory)
    val views = new ViewIndex(new RAMDirectory)

    new SearchEngine(settings, values, typeDefs, modules, views)
  }
}

class SearchEngine private[searchEngine] (
    val settings: Settings,
    private[scaps] val valueIndex: ValueIndex,
    private[scaps] val typeIndex: TypeIndex,
    private[scaps] val moduleIndex: ModuleIndex,
    private[scaps] val viewIndex: ViewIndex) extends Logging {

  private val indexes = List(valueIndex, typeIndex, moduleIndex, viewIndex)

  def index(entities: Seq[Definition]): Try[Unit] =
    Try {
      def values = entities.collect { case v: ValueDef => v }
      def types = entities.collect { case t: TypeDef => t }
      def views = entities.collect { case t: ViewDef => t }
      def modules = entities.foldLeft(Set[Module]()) { (ms, d) =>
        ms + d.module
      }

      val f = Future.sequence(List(
        Future { valueIndex.addEntities(values).get },
        Future { typeIndex.addEntities(types).get },
        Future { viewIndex.addEntities(views).get }))

      Await.result(f, settings.index.timeout)

      moduleIndex.addEntities(modules.toSeq).get
    }

  def finalizeIndex(): Try[Unit] =
    Try {
      resetAnalyzers()

      updateTypeFrequencies().get

      resetAnalyzers()
    }

  private def updateTypeFrequencies(): Try[Unit] =
    Try {
      logger.info(s"Start updating type frequencies for modules ${moduleIndex.allEntities().get}")

      val allModulesAnalyzer = analyzer(indexedModules().get.map(_.moduleId).toSet)

      val tfs = TypeFrequencies(
        allModulesAnalyzer.apply,
        valueIndex.allEntities().get,
        settings.index.typeFrequenciesSampleSize)

      typeIndex.updateTypeFrequencies(tfs)

      logger.info(s"Type frequencies have been updated")
    }

  def deleteSourceArtifact(artifactPath: String): Try[Unit] =
    valueIndex.deleteSourceArtifact(artifactPath)

  def deleteModule(module: Module): Try[Unit] = Try {
    valueIndex.deleteEntitiesIn(module).get
    typeIndex.deleteEntitiesIn(module).get
    moduleIndex.deleteModule(module).get
    viewIndex.deleteEntitiesIn(module).get
  }

  /**
   * Yields a list of definitions matching `query` and the `moduleIds` filter or an error if the query
   * could not be successfully resolved.
   *
   * Concurrent calls to this methods are save. But calling `search` while another operation is running
   * (particularly `indexEntities`) may result in an exception.
   */
  def search(query: String, moduleIds: Set[String] = Set()): Try[QueryError \/ Seq[Result[ValueDef]]] = Try {
    val moduleAnalyzer = analyzer(moduleIds)

    for {
      analyzed <- moduleAnalyzer(query)
      results <- valueIndex.find(analyzed, moduleIds).get
    } yield {
      logger.debug(s"""query "${query}" expanded to:\n"${analyzed.prettyPrint}" """)
      results
    }
  }

  def indexedModules(): Try[Seq[Module]] =
    moduleIndex.allEntities()

  def resetIndexes(): Try[Unit] = Try {
    for {
      index <- indexes
    } index.resetIndex().get
  }

  private var analyzer: Set[String] => QueryAnalyzer = null

  private def resetAnalyzers() = {
    analyzer = Memo.mutableHashMapMemo {
      (moduleIds: Set[String]) =>
        def findClassBySuffix(suffix: String) =
          typeIndex.findTypeDefsBySuffix(suffix, moduleIds).get

        new QueryAnalyzer(
          settings,
          Memo.mutableHashMapMemo((findClassBySuffix _)),
          Memo.mutableHashMapMemo(typeIndex.termFrequency(_, moduleIds).get),
          Memo.mutableHashMapMemo(viewIndex.findViews(_, moduleIds).get))
          .favorTypesMatching(Pattern.compile("""scala\..*"""))
          .favorTypesMatching(Pattern.compile("""(scala\.([^\.#]+))|java\.lang\.String"""))
    }
  }

  resetAnalyzers()
}

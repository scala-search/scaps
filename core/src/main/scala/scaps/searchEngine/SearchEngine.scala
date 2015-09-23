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

  /**
   * Names from the `scala` root package are favored over other names and all names in
   * the `scala` namespace have a higher priority. This allows queries like `List => Future`.
   */
  private[searchEngine] def favorScalaStdLib(candidates: Seq[TypeDef]) = {
    // typeDefs in root `scala` namespace and java.lang.String are always favored
    val firstPrioPattern = """(scala\.([^\.#]+))|java\.lang\.String"""
    // unambiguous names from the `scala` namespace are also priotized over names from other namespaces
    val secondPrioPattern = """scala\..*"""

    candidates.filter(_.name.matches(firstPrioPattern)) match {
      case Seq(fav) => Seq(fav)
      case _ => candidates.filter(_.name.matches(secondPrioPattern)) match {
        case Seq(fav) => Seq(fav)
        case _        => candidates
      }
    }
  }
}

class SearchEngine private[searchEngine] (
  val settings: Settings,
  private[scaps] val valueIndex: ValueIndex,
  private[scaps] val typeIndex: TypeIndex,
  private[scaps] val moduleIndex: ModuleIndex,
  private[scaps] val viewIndex: ViewIndex) extends Logging {

  private val indexes = List(valueIndex, typeIndex, moduleIndex, viewIndex)

  def indexEntities(modulesWithEntities: Seq[(Module, () => Seq[Definition])]): Try[Unit] =
    Try {
      for {
        (m, es) <- modulesWithEntities
      } {
        indexModule(m, es()).get
        logger.info(s"Successfully indexed $m")
      }

      updateTypeFrequencies().get
    }

  def indexEntities(module: Module, entities: Seq[Definition]): Try[Unit] =
    Try {
      indexModule(module, entities).get
      updateTypeFrequencies().get
    }

  private def indexModule(module: Module, entities: Seq[Definition]): Try[Unit] =
    Try {
      analyzers = Map()

      deleteModule(module).get

      def setModule(t: ValueDef) =
        if (module == Module.Unknown)
          t
        else
          t.copy(module = module)

      val entitiesWithSyntheticTypes = entities ++ List(
        TypeDef(TypeRef.Unknown.name, Nil, Nil),
        TypeDef(TypeRef.Implicit.name, TypeParameter("T", Invariant) :: Nil, Nil))

      val valuesWithModule = entitiesWithSyntheticTypes
        .collect { case t: ValueDef => setModule(t) }
      val typeDefsWithModule = entitiesWithSyntheticTypes
        .collect { case c: TypeDef => c.copy(referencedFrom = Set(module)) }
      val viewDefWithModule = entitiesWithSyntheticTypes
        .collect{case v: ViewDef => v.copy(modules = Set(module))}

      val f = Future.sequence(List(
        Future { valueIndex.addEntities(valuesWithModule).get },
        Future { typeIndex.addEntities(typeDefsWithModule).get },
        Future { viewIndex.addEntities(viewDefWithModule).get }))

      Await.result(f, settings.index.timeout)

      moduleIndex.addEntities(Seq(module)).get
    }

  private def updateTypeFrequencies(): Try[Unit] =
    Try {
      logger.info(s"Start updating type frequencies for modules ${moduleIndex.allEntities().get}")

      val tfs = TypeFrequencies(
        viewIndex.findAlternativesWithDistance(_).get.map(_._1),
        valueIndex.allEntities().get,
        settings.index.typeFrequenciesSampleSize)

      val adjustedTfs = TypeFrequencies.adjustInvariantTopType(tfs)

      val typeDefsWithFrequencies = typeIndex.allTypeDefs().get.map { cls =>
        def freq(v: Variance) = adjustedTfs((v, cls.name))

        cls.copy(typeFrequency = Map(
          Covariant -> freq(Covariant),
          Contravariant -> freq(Contravariant),
          Invariant -> freq(Invariant)))
      }

      typeIndex.replaceAllEntities(typeDefsWithFrequencies)

      logger.info(s"Type frequencies have been updated")
    }

  def deleteModule(module: Module): Try[Unit] = Try {
    valueIndex.deleteEntitiesIn(module).get
    typeIndex.deleteEntitiesIn(module).get
    moduleIndex.deleteModule(module).get
  }

  /**
   * Yields a list of definitions matching `query` and the `moduleIds` filter or an error if the query
   * could not be successfully resolved.
   *
   * Concurrent calls to this methods are save. But calling `search` while another operation is running
   * (particularly `indexEntities`) may result in an exception.
   */
  def search(query: String, moduleIds: Set[String] = Set()): Try[QueryError \/ Seq[ValueDef]] = Try {
    for {
      parsed <- QueryParser(query)
      analyzed <- analyzeQuery(moduleIds, parsed).get
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

  private var analyzers: Map[Set[String], QueryAnalyzer] = Map()

  private def analyzeQuery(moduleIds: Set[String], raw: RawQuery) = Try {
    def findClassBySuffix(suffix: String) =
      typeIndex.findTypeDefsBySuffix(suffix, moduleIds).get

    val analyzer = analyzers.get(moduleIds).fold {
      val analyzer = new QueryAnalyzer(
        settings.query,
        Memo.mutableHashMapMemo((findClassBySuffix _) andThen (SearchEngine.favorScalaStdLib _)),
        Memo.mutableHashMapMemo(viewIndex.findAlternativesWithDistance(_).get))

      analyzers += (moduleIds -> analyzer)

      analyzer
    } { identity }

    analyzer(raw).swapped(_.flatMap {
      case _: NameNotFound =>
        raw match {
          case RawQuery.Full("", tpe) if tpe.args.length == 0 =>
            analyzer(RawQuery.Keywords(tpe.name)).swap
        }
      case e =>
        \/.right(e)
    })
  }
}

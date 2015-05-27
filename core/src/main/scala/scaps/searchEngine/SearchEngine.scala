package scaps.searchEngine

import java.io.File

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

import org.apache.lucene.store.FSDirectory
import org.apache.lucene.store.RAMDirectory

import scalaz.{ \/ => \/ }
import scaps.searchEngine.index.ClassIndex
import scaps.searchEngine.index.ModuleIndex
import scaps.searchEngine.index.TermsIndex
import scaps.searchEngine.index.ViewIndex
import scaps.searchEngine.index.TypeFrequencies
import scaps.searchEngine.queries.QueryAnalyzer
import scaps.searchEngine.queries.QueryParser
import scaps.searchEngine.queries.RawQuery
import scaps.settings.Settings
import scaps.utils.Logging
import scaps.utils.SampleSeqOps
import scaps.webapi.ClassEntity
import scaps.webapi.Contravariant
import scaps.webapi.Covariant
import scaps.webapi.Entity
import scaps.webapi.Invariant
import scaps.webapi.Module
import scaps.webapi.TermEntity
import scaps.webapi.TypeEntity
import scaps.webapi.Variance

object SearchEngine {
  def apply(settings: Settings): Try[SearchEngine] = Try {
    def createDir(path: File) = {
      path.mkdirs()
      FSDirectory.open(path)
    }

    val terms = new TermsIndex(createDir(settings.index.termsDir), settings)
    val classes = new ClassIndex(createDir(settings.index.classesDir), settings)
    val modules = new ModuleIndex(createDir(settings.index.modulesDir))
    val views = new ViewIndex(createDir(settings.index.viewsDir))

    new SearchEngine(settings, terms, classes, modules, views)
  }

  def inMemory(settings: Settings): SearchEngine = {
    val terms = new TermsIndex(new RAMDirectory, settings)
    val classes = new ClassIndex(new RAMDirectory, settings)
    val modules = new ModuleIndex(new RAMDirectory)
    val views = new ViewIndex(new RAMDirectory)

    new SearchEngine(settings, terms, classes, modules, views)
  }

  /**
   * Names from the `scala` root package are favored over other names and all names in
   * the `scala` namespace have a higher priority. This allows queries like `List => Future`.
   */
  def favorScalaStdLib(candidates: Seq[ClassEntity]) = {
    // classes in root `scala` namespace and java.lang.String are always favored
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
  private[scaps] val termsIndex: TermsIndex,
  private[scaps] val classesIndex: ClassIndex,
  private[scaps] val moduleIndex: ModuleIndex,
  private[scaps] val viewIndex: ViewIndex) extends Logging {

  private val indexes = List(termsIndex, classesIndex, moduleIndex, viewIndex)

  def indexEntities(module: Module, entities: Seq[Entity], batchMode: Boolean = false)(implicit ec: ExecutionContext): Try[Unit] =
    Try {
      deleteModule(module).get

      def setModule(t: TermEntity) =
        if (module == Module.Unknown)
          t
        else
          t.copy(module = module)

      val termsWithModule = entities.collect { case t: TermEntity => setModule(t) }
      val classesWithModule = entities
        .collect { case c: ClassEntity => c.copy(referencedFrom = Set(module)) }

      val allClasses = classesWithModule :+ ClassEntity(TypeEntity.Unknown.name, Nil, Nil, referencedFrom = Set(module))

      val views = allClasses.flatMap(View.fromClass)

      val f = Future.sequence(List(
        Future { termsIndex.addEntities(termsWithModule).get },
        Future { classesIndex.addEntities(allClasses).get },
        Future { viewIndex.addEntities(views).get }))

      Await.result(f, settings.index.timeout)

      if (!batchMode) {
        updateTypeFrequencies().get
      }

      moduleIndex.addEntities(Seq(module)).get
    }

  def updateTypeFrequencies(): Try[Unit] =
    Try {
      logger.info(s"Start updating type frequencies for modules ${moduleIndex.allModules().get}")

      val tfs = TypeFrequencies(
        viewIndex.findViews(_).get,
        termsIndex.allTerms().get.sample(settings.index.typeFrequenciesSampleSize))

      val classesWithFrequencies = classesIndex.allClasses().get.map { cls =>
        def freq(v: Variance) = tfs((v, cls.name))

        cls.copy(typeFrequency = Map(
          Covariant -> freq(Covariant),
          Contravariant -> freq(Contravariant),
          Invariant -> freq(Invariant)))
      }

      classesIndex.addEntities(classesWithFrequencies).get

      logger.info(s"Type frequencies have been updated")
    }

  def deleteModule(module: Module): Try[Unit] = Try {
    termsIndex.deleteEntitiesIn(module).get
    classesIndex.deleteEntitiesIn(module).get
    moduleIndex.deleteModule(module).get
  }

  def search(query: String, moduleIds: Set[String] = Set()): Try[QueryError \/ Seq[TermEntity]] = Try {
    for {
      parsed <- QueryParser(query)
      analyzed <- analyzeQuery(moduleIds, parsed).get
      results <- termsIndex.find(analyzed, moduleIds).get
    } yield {
      logger.debug(s"""query "${query}" expanded to "${analyzed}" """)
      results
    }
  }

  def indexedModules(): Try[Seq[Module]] =
    moduleIndex.allModules()

  def resetIndexes(): Try[Unit] = Try {
    for {
      index <- indexes
    } index.resetIndex().get
  }

  private def analyzeQuery(moduleIds: Set[String], raw: RawQuery) = Try {
    def findClassBySuffix(suffix: String) =
      (classesIndex.findClassBySuffix(suffix, moduleIds).get)

    val analyzer = new QueryAnalyzer(
      settings,
      (findClassBySuffix _) andThen (SearchEngine.favorScalaStdLib _),
      viewIndex.findViews(_).get)

    analyzer(raw)
  }
}

package scaps.searchEngine

import java.io.File
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scaps.webapi.ClassEntity
import scaps.webapi.Entity
import scaps.webapi.TermEntity
import scaps.searchEngine.index.ClassIndex
import scaps.searchEngine.index.TermsIndex
import scaps.searchEngine.queries.QueryAnalyzer
import scaps.searchEngine.queries.QueryParser
import scaps.settings.Settings
import scaps.utils.Logging
import scala.util.Try
import org.apache.lucene.store.FSDirectory
import scalaz.\/
import scaps.webapi.Module
import scaps.searchEngine.index.ModuleIndex
import scaps.webapi.ClassEntity
import scaps.webapi.TypeEntity
import scala.concurrent.Await
import scala.concurrent.duration._

object SearchEngine {
  def apply(settings: Settings): Try[SearchEngine] = Try {
    def createDir(path: File) = {
      path.mkdirs()
      FSDirectory.open(path)
    }

    val terms = new TermsIndex(createDir(settings.index.termsDir), settings)
    val classes = new ClassIndex(createDir(settings.index.classesDir), settings)
    val modules = new ModuleIndex(createDir(settings.index.modulesDir))

    new SearchEngine(settings, terms, classes, modules)
  }

  /**
   * Names from the `scala` root package are favored over other names and all names in
   * the `scala` namespace have a higher priority. This allows queries like `List => Future`.
   */
  def favorScalaStdLib(results: Try[Seq[ClassEntity]]) =
    for {
      candidates <- results
    } yield {
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
  private[scaps] val moduleIndex: ModuleIndex) extends Logging {

  def indexEntities(module: Module, entities: Seq[Entity])(implicit ec: ExecutionContext): Try[Unit] =
    Try {
      deleteModule(module).get

      def setModule(t: TermEntity) =
        if (module == Module.Unknown)
          t
        else
          t.copy(module = module)

      val termsWithModule = entities.collect { case t: TermEntity => setModule(t) }
      val classesWithModule = entities.collect { case c: ClassEntity => c.copy(referencedFrom = Set(module)) }

      val f = Future.sequence(List(
        Future { termsIndex.addEntities(termsWithModule).get },
        Future { classesIndex.addEntities(classesWithModule).get }))

      Await.result(f, settings.index.timeout)

      moduleIndex.addEntities(Seq(module)).get
    }

  def deleteModule(module: Module): Try[Unit] = Try {
    termsIndex.deleteEntitiesIn(module).get
    classesIndex.deleteEntitiesIn(module).get
    moduleIndex.deleteModule(module).get
  }

  def search(query: String, moduleIds: Set[String] = Set()): Try[QueryError \/ Seq[TermEntity]] = Try {
    for {
      parsed <- QueryParser(query)
      analyzed <- analyzer(moduleIds)(parsed).get
      results <- termsIndex.find(analyzed, moduleIds).get
    } yield {
      logger.debug(s"""query "${query}" expanded to "${analyzed.fingerprint.mkString(" ")}" """)
      results
    }
  }

  def indexedModules(): Try[Seq[Module]] =
    moduleIndex.allModules()

  def resetIndexes(): Try[Unit] = for {
    _ <- termsIndex.resetIndex()
    _ <- classesIndex.resetIndex()
    _ <- moduleIndex.resetIndex()
  } yield ()

  private def analyzer(moduleIds: Set[String]) = {
    def findClassBySuffix(suffix: String) =
      (classesIndex.findClassBySuffix(suffix, moduleIds))

    new QueryAnalyzer(
      settings.query,
      (findClassBySuffix _) andThen (SearchEngine.favorScalaStdLib _),
      classesIndex.findSubClasses _)
  }
}

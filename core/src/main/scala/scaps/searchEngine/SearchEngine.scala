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
  val termsIndex: TermsIndex,
  val classesIndex: ClassIndex,
  val moduleIndex: ModuleIndex) extends Logging {

  val analyzer = new QueryAnalyzer(
    settings.query,
    (classesIndex.findClassBySuffix _) andThen (SearchEngine.favorScalaStdLib _),
    classesIndex.findSubClasses _)

  def indexEntities(module: Module, entities: Seq[Entity])(implicit ec: ExecutionContext): Future[Unit] =
    Future {
      termsIndex.deleteEntitiesIn(module).get
    }.flatMap { _ =>
      def setModule(t: TermEntity) =
        if (module == Module.Unknown)
          t
        else
          t.copy(module = module)

      Future.sequence(List(
        Future { termsIndex.addEntities(entities.collect { case t: TermEntity => setModule(t) }) },
        Future { classesIndex.addEntities(entities.collect { case c: ClassEntity => c }) }))
        .map { results =>
          results.foreach(_.get)
          moduleIndex.addEntities(Seq(module)).get
        }
    }

  def search(query: String, moduleIds: Set[String] = Set()): Try[QueryError \/ Seq[TermEntity]] = Try {
    for {
      parsed <- QueryParser(query)
      analyzed <- analyzer(parsed).get
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
}

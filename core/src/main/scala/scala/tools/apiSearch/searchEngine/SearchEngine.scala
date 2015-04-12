package scala.tools.apiSearch.searchEngine

import java.io.File

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.tools.apiSearch.model.ClassEntity
import scala.tools.apiSearch.model.Entity
import scala.tools.apiSearch.model.TermEntity
import scala.tools.apiSearch.searchEngine.index.ClassIndex
import scala.tools.apiSearch.searchEngine.index.TermsIndex
import scala.tools.apiSearch.searchEngine.queries.QueryAnalyzer
import scala.tools.apiSearch.searchEngine.queries.QueryParser
import scala.tools.apiSearch.settings.Settings
import scala.util.Try

import org.apache.lucene.store.FSDirectory

import scalaz.\/

object SearchEngine {
  def apply(settings: Settings): Try[SearchEngine] = Try {
    def createDir(path: File) = {
      path.mkdirs()
      FSDirectory.open(path)
    }

    new SearchEngine(settings,
      new TermsIndex(createDir(settings.index.termsDir), settings),
      new ClassIndex(createDir(settings.index.classesDir), settings))
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

class SearchEngine private (val settings: Settings, val termsIndex: TermsIndex, val classesIndex: ClassIndex) {
  val analyzer = new QueryAnalyzer(
    settings.query,
    (classesIndex.findClassBySuffix _) andThen (SearchEngine.favorScalaStdLib _),
    classesIndex.findSubClasses _)

  def deleteIndexes() = for {
    _ <- termsIndex.delete()
    _ <- classesIndex.delete()
  } yield ()

  def indexEntities(entities: Stream[Entity])(implicit ec: ExecutionContext): Future[Unit] = {
    val f1 = Future { termsIndex.addEntities(entities.collect { case t: TermEntity => t }) }
    val f2 = Future { classesIndex.addEntities(entities.collect { case c: ClassEntity => c }) }
    Future.sequence(f1 :: f2 :: Nil).map(_.foreach(_.get))
  }

  def search(query: String): Try[QueryError \/ Seq[TermEntity]] = Try {
    for {
      parsed <- QueryParser(query)
      analyzed <- analyzer(parsed).get
      results <- termsIndex.find(analyzed).get
    } yield results
  }
}

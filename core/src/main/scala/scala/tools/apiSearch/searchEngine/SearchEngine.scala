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

import scalaz.syntax.validation.ToValidationOps

object SearchEngine {
  def apply(settings: Settings): Try[SearchEngine] = Try {
    def createDir(path: File) = {
      path.mkdirs()
      FSDirectory.open(path)
    }

    new SearchEngine(settings,
      new TermsIndex(createDir(settings.index.termsDir), settings),
      new ClassIndex(createDir(settings.index.classesDir)))
  }
}

class SearchEngine private (settings: Settings, val termsIndex: TermsIndex, val classesIndex: ClassIndex) {
  def reset() = for {
    _ <- termsIndex.delete()
    _ <- classesIndex.delete()
  } yield ()

  def indexEntities(entities: Stream[Entity])(implicit ec: ExecutionContext): Future[Unit] = {
    val f1 = Future { termsIndex.addEntities(entities.collect { case t: TermEntity => t }) }
    val f2 = Future { classesIndex.addEntities(entities.collect { case c: ClassEntity => c }) }
    Future.sequence(f1 :: f2 :: Nil).map(_.foreach(_.get))
  }

  def search(query: String): Try[QueryAnalyzer.ErrorsOr[Seq[TermEntity]]] = Try {
    val analyzer = new QueryAnalyzer(settings.query, classesIndex.findClass _, classesIndex.findSubClasses _)
    QueryParser(query).fold({ msg =>
      QueryAnalyzer.SyntaxError(msg).failureNel
    }, { raw =>
      analyzer(raw).get.map { query =>
        termsIndex.find(query).get
      }
    })

  }
}

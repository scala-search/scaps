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

import scalaz._
import scalaz.Validation.FlatMap.ValidationFlatMapRequested
import scalaz.ValidationNel

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
}

class SearchEngine private (val settings: Settings, val termsIndex: TermsIndex, val classesIndex: ClassIndex) {
  def deleteIndexes() = for {
    _ <- termsIndex.delete()
    _ <- classesIndex.delete()
  } yield ()

  def indexEntities(entities: Stream[Entity])(implicit ec: ExecutionContext): Future[Unit] = {
    val f1 = Future { termsIndex.addEntities(entities.collect { case t: TermEntity => t }) }
    val f2 = Future { classesIndex.addEntities(entities.collect { case c: ClassEntity => c }) }
    Future.sequence(f1 :: f2 :: Nil).map(_.foreach(_.get))
  }

  def search(query: String): Try[ValidationNel[QueryError, Seq[TermEntity]]] = Try {
    def analyzer = new QueryAnalyzer(settings.query, classesIndex.findClassBySuffix _, classesIndex.findSubClasses _)

    for {
      parsed <- QueryParser(query).toValidationNel
      analyzed <- analyzer(parsed).get
      results <- termsIndex.find(analyzed).get.toValidationNel
    } yield results
  }
}

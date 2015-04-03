package scala.tools.apiSearch

import java.io.File
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.tools.apiSearch.model.ClassEntity
import scala.tools.apiSearch.model.Entity
import scala.tools.apiSearch.model.TermEntity
import scala.tools.apiSearch.settings.Settings
import org.apache.lucene.store.FSDirectory
import scala.tools.apiSearch.searching.QueryParser
import scala.tools.apiSearch.searching.QueryAnalyzer
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import scala.util.Try
import scala.tools.apiSearch.index.ClassIndex
import scala.tools.apiSearch.index.TermsIndex

class SearchEngine(settings: Settings) {
  lazy val (termsIndex, classesIndex) = {
    def createDir(path: File) = {
      path.mkdirs()
      FSDirectory.open(path)
    }

    (new TermsIndex(createDir(settings.index.termsDir), settings),
      new ClassIndex(createDir(settings.index.classesDir)))
  }

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
    val analyzer = QueryAnalyzer(settings.query, classesIndex)
    QueryParser(query).fold({ msg =>
      QueryAnalyzer.SyntaxError(msg).failureNel
    }, { raw =>
      analyzer(raw).get.map { query =>
        termsIndex.find(query).get
      }
    })

  }
}

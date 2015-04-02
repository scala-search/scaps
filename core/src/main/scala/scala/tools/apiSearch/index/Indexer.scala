package scala.tools.apiSearch.index

import java.io.File

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.tools.apiSearch.model.ClassEntity
import scala.tools.apiSearch.model.Entity
import scala.tools.apiSearch.model.TermEntity
import scala.tools.apiSearch.settings.Settings

import org.apache.lucene.store.FSDirectory

class Indexer(settings: Settings) {
  lazy val (termsIndex, classesIndex) = {
    def createDir(path: File) = {
      path.mkdirs()
      FSDirectory.open(path)
    }

    (new TermsIndex(createDir(settings.index.termsDir), settings.query),
      new ClassIndex(createDir(settings.index.classesDir)))
  }

  def reset() = for {
    _ <- termsIndex.delete()
    _ <- classesIndex.delete()
  } yield ()

  def index(entities: Stream[Entity])(implicit ec: ExecutionContext): Future[Unit] = {
    val f1 = Future { termsIndex.addEntities(entities.collect { case t: TermEntity => t }) }
    val f2 = Future { classesIndex.addEntities(entities.collect { case c: ClassEntity => c }) }
    Future.sequence(f1 :: f2 :: Nil).map(_.foreach(_.get))
  }
}

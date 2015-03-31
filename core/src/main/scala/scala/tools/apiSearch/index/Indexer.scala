package scala.tools.apiSearch.index

import org.apache.lucene.store.FSDirectory
import scala.tools.nsc.interactive.Global
import scala.tools.apiSearch.model._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.tools.apiSearch.settings.IndexSettings
import java.io.File

class Indexer(settings: IndexSettings) {
  lazy val (termsIndex, classesIndex) = {
    def createDir(path: File) = {
      path.mkdirs()
      FSDirectory.open(path)
    }

    (new TermsIndex(createDir(settings.termsDir)), new ClassIndex(createDir(settings.classesDir)))
  }

  def reset() = for {
    _ <- termsIndex.delete()
    _ <- classesIndex.delete()
  } yield ()

  def index(entities: Stream[Entity])(implicit ec: ExecutionContext): Future[Unit] = {
    val f1 = Future { termsIndex.addEntities(entities.collect { case t: TermEntity => t }) }
    val f2 = Future { classesIndex.addEntities(entities.collect { case c: ClassEntity => c }) }
    Future.sequence(f1 :: f2 :: Nil).map(_ => ())
  }
}

package scala.tools.apiSearch.index

import org.apache.lucene.store.FSDirectory
import java.nio.file.Paths
import scala.reflect.io.Path
import scala.tools.nsc.interactive.Global
import scala.tools.apiSearch.model._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.Try

class Indexer(indexDir: Path) {
  lazy val (termsIndex, classesIndex) = {
    def createDir(path: Path) = {
      path.jfile.mkdirs()
      FSDirectory.open(path.jfile)
    }

    (new TermsIndex(createDir(indexDir / "terms")), new ClassIndex(createDir(indexDir / "classes")))
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

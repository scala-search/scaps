package scaps.webservice.actors

import java.io.File

import scala.util.control.NonFatal

import ActorProtocol._
import akka.actor.Actor
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.event.Logging
import scalaz.std.stream.streamInstance
import scaps.featureExtraction.CompilerUtils
import scaps.featureExtraction.ExtractionError
import scaps.featureExtraction.JarExtractor
import scaps.searchEngine.NameAmbiguous
import scaps.searchEngine.NameNotFound
import scaps.searchEngine.SearchEngine
import scaps.searchEngine.SyntaxError
import scaps.searchEngine.TooUnspecific
import scaps.searchEngine.UnexpectedNumberOfTypeArgs
import scaps.settings.Settings
import scaps.utils.TraversableOps
import scaps.webapi.IndexBusy
import scaps.webapi.IndexReady
import scaps.webapi.IndexStatus

object Director {
  def props(settings: Settings)(
    indexWorkerProps: SearchEngine => Props = IndexWorkerActor.props _,
    searcherProps: SearchEngine => Props = Searcher.props _) =
    Props(classOf[Director], settings, indexWorkerProps, searcherProps)
}

/**
 * Manages the state of an instance of the search engine. Allows concurrent searching
 * while another index is being built.
 */
class Director(baseSettings: Settings, indexWorkerProps: SearchEngine => Props, searcherProps: SearchEngine => Props) extends Actor {
  import context._

  val logger = Logging(system, this)

  def receive = {
    def ready(searchEngine: SearchEngine, indexNo: Int): Receive = {
      logger.info(s"Search engine uses index at ${searchEngine.settings.index.indexDir}")

      val indexedModules = searchEngine.indexedModules().get
      val status = IndexReady(indexedModules, Nil)

      {
        case i: Index =>
          val workerIndexPath = mkIndexPath(baseSettings.index.indexDir, indexNo + 1)
          val workerSettings = baseSettings.copy(index = baseSettings.index.copy(indexDir = workerIndexPath))
          val workerEngine = SearchEngine(workerSettings).get
          val indexWorker = actorOf(indexWorkerProps(workerEngine))

          indexWorker ! i
          sender ! true

          become(indexing(searchEngine, IndexBusy(i.jobs.map(_.module), status.indexedModules, Nil), indexNo))
        case s: Search =>
          val searcher = actorOf(searcherProps(searchEngine))
          searcher.tell(s, sender)
        case GetStatus =>
          sender ! status
      }
    }

    def indexing(searchEngine: SearchEngine, status: IndexStatus, indexNo: Int): Receive = {
      case i: Index =>
        sender ! false
      case Indexed(_, _, updatedEngine) =>
        become(ready(updatedEngine, indexNo + 1))
      case s: Search =>
        val searcher = actorOf(searcherProps(searchEngine))
        searcher.tell(s, sender)
      case GetStatus =>
        sender ! status
    }

    val (indexPath, indexNo) = findCurrentIndexWithNo(new File(baseSettings.index.indexDir))
    val currentSettings = baseSettings.copy(index = baseSettings.index.copy(indexDir = indexPath))
    val searchEngine = SearchEngine(currentSettings).get

    ready(searchEngine, indexNo)
  }

  val indexName = """index-(\d+)""".r

  def mkIndexPath(basePath: String, indexNo: Int): String =
    s"$basePath/index-$indexNo"

  def findCurrentIndexWithNo(baseDir: File): (String, Int) = {
    baseDir.listFiles().toSeq
      .filter(f => f.isDirectory())
      .flatMap(f => f.getName match {
        case indexName(n) => Some((f.getAbsolutePath, n.toInt))
        case _            => None
      })
      .maxByOpt(_._2)
      .getOrElse((mkIndexPath(baseDir.getAbsolutePath, 0), 0))
  }
}

object IndexWorkerActor {
  def props(searchEngine: SearchEngine) =
    Props(classOf[IndexWorkerActor], searchEngine)
}

/**
 * Indexes source files.
 */
class IndexWorkerActor(searchEngine: SearchEngine) extends Actor {
  import scala.concurrent.ExecutionContext.Implicits.global

  val logger = Logging(context.system, this)

  def receive = {
    case i @ Index(jobs, classpath) =>
      val requestor = sender

      val error = try {
        CompilerUtils.withCompiler(classpath) { compiler =>
          val modulesWithEntities = jobs.map { job =>
            (job.module, () => {
              val extractor = new JarExtractor(compiler)

              logger.info(s"start indexing ${job.module.moduleId} (${job.artifactPath})")

              val entitiesWithErrors = extractor(new File(job.artifactPath))
              ExtractionError.logErrors(entitiesWithErrors, logger.info(_))
            })
          }

          searchEngine.indexEntities(modulesWithEntities).get
        }

        None
      } catch {
        case NonFatal(e) =>
          logger.error(s"Indexing ${jobs} threw $e")
          Some(e)
      }

      requestor ! Indexed(jobs, error, searchEngine)
  }
}

object Searcher {
  def props(searchEngine: SearchEngine) =
    Props(classOf[Searcher], searchEngine)
}

class Searcher(searchEngine: SearchEngine) extends Actor {
  import scaps.searchEngine._

  def receive = {
    case Search(q, moduleIds, noResults, offset) =>
      val res = searchEngine.search(q, moduleIds).get.map {
        case terms => terms.drop(offset).take(noResults)
      }.leftMap {
        case SyntaxError(msg) =>
          msg
        case NameNotFound(name) =>
          s"Type ${name} not found"
        case NameAmbiguous(name, candidates) =>
          s"Type ${name} is ambiguous. Possible candidates: ${candidates.map(_.name).mkString(", ")}"
        case UnexpectedNumberOfTypeArgs(raw, n) =>
          s"$raw has wrong number of arguments ($n expected)"
        case TooUnspecific() =>
          s"Query too unspecific consider using wildcards '_' instead of 'Any' types"
      }
      sender ! res
      context.stop(self)
  }
}

package scaps.webservice.actors

import java.io.File
import scala.concurrent.Await
import scala.concurrent.duration._
import scaps.featureExtraction.CompilerUtils
import scaps.featureExtraction.ExtractionError
import scaps.featureExtraction.JarExtractor
import scaps.searchEngine.SearchEngine
import scaps.settings.Settings
import scala.util.Try
import akka.actor.Actor
import akka.actor.FSM
import akka.actor.Props
import akka.event.Logging
import akka.actor.actorRef2Scala
import scala.concurrent.Future
import akka.actor.ActorRef
import scalaz.\/
import scalaz.std.stream._
import java.util.concurrent.TimeoutException
import scaps.webapi.IndexStatus
import scaps.webapi.IndexReady
import scaps.webapi.IndexBusy
import scaps.webapi.Module
import scala.util.control.NonFatal
import akka.event.LoggingReceive

object SearchEngineActor {
  def props(searchEngine: SearchEngine)(
    indexWorkerProps: Props = IndexWorkerActor.props(searchEngine),
    searcherProps: Props = Searcher.props(searchEngine)) =
    Props(classOf[SearchEngineActor], searchEngine, indexWorkerProps, searcherProps)
}

/**
 * Manages the state of an instance of the search engine.
 */
class SearchEngineActor(searchEngine: SearchEngine, indexWorkerProps: Props, searcherProps: Props) extends Actor {
  import SearchEngineProtocol._
  import context._

  val logger = Logging(context.system, this)

  def receive = {
    val indexWorker = actorOf(indexWorkerProps, "indexWorker")

    def ready(indexedModules: Set[Module], indexErrors: Seq[String]): Receive = {
      logger.info(s"search engine ready with modules $indexedModules")

      {
        case i: Index if i.forceReindex == false =>
          enqueueJobIfNewModule(i, Nil, indexedModules, indexErrors)
        case i: Index =>
          enqueueJob(i, Nil, indexedModules, indexErrors)
        case s: Search =>
          val searcher = actorOf(searcherProps)
          searcher.tell(s, sender)
        case GetStatus =>
          sender ! IndexReady(indexedModules.toSeq, indexErrors)
        case Reset =>
          searchEngine.resetIndexes().get
          become(ready(Set(), Nil))
      }
    }

    def indexing(queue: Seq[Index], indexedModules: Set[Module], indexErrors: Seq[String]): Receive = {
      case i: Index if i.forceReindex == false =>
        enqueueJobIfNewModule(i, queue, indexedModules, indexErrors)
      case i: Index =>
        enqueueJob(i, queue, indexedModules, indexErrors)
      case res @ Indexed(indexJob, error) if indexJob == queue.head =>
        val errors = indexErrors ++ error.map(_.toString())
        val indexed = indexedModules ++
          (if (error.isDefined) Nil else Seq(indexJob.module))

        if (queue.tail.isEmpty) {
          indexWorker ! FinalizeIndexes
          become(finalizing(indexed, errors))
        } else {
          indexWorker ! queue.tail.head
          become(indexing(queue.tail, indexed, errors))
        }
      case Indexed(j, _) =>
        throw new IllegalStateException()
      case _: Search =>
        sender ! \/.left(s"Cannot search while index is being built. ${queue.size} modules left.")
      case GetStatus =>
        sender ! IndexBusy(queue.map(_.module), indexedModules.toSeq, indexErrors)
      case Reset =>
        become(resetting())
    }

    def finalizing(indexedModules: Set[Module], indexErrors: Seq[String]): Receive = {
      case Finalized =>
        become(ready(indexedModules, indexErrors))
      case _: Search =>
        sender ! \/.left(s"Cannot search while index is updating.")
      case GetStatus =>
        sender ! IndexBusy(Nil, indexedModules.toSeq, indexErrors)
    }

    def resetting(): Receive = {
      case i: Indexed =>
        searchEngine.resetIndexes().get
        become(ready(Set(), Nil))
      case _: Search =>
        sender ! \/.left(s"Cannot search while index is resetting.")
      case GetStatus =>
        sender ! IndexBusy(Nil, Nil, Nil)
    }

    def enqueueJob(indexJob: Index, queue: Seq[Index], indexedModules: Set[Module], indexErrors: Seq[String]) = {
      if (queue.isEmpty) {
        indexWorker ! indexJob
      }

      become(indexing(queue :+ indexJob, indexedModules, indexErrors))
    }

    def enqueueJobIfNewModule(indexJob: Index, queue: Seq[Index], indexedModules: Set[Module], indexErrors: Seq[String]) = {
      val allModules = queue.map(_.module) ++ indexedModules

      if (allModules.contains(indexJob.module)) {
        logger.debug(s"Drop index job $indexJob")
        sender ! Indexed(indexJob, None)
      } else {
        logger.debug(s"Enqueue unenforced index job $indexJob")
        enqueueJob(indexJob, queue, indexedModules, indexErrors)
      }
    }

    val indexedModules = searchEngine.indexedModules().get

    ready(indexedModules.toSet, Nil)
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
  import SearchEngineProtocol._
  import scala.concurrent.ExecutionContext.Implicits.global

  val logger = Logging(context.system, this)

  def receive = {
    case FinalizeIndexes =>
      searchEngine.finalizeIndexes().get
      sender ! Finalized

    case i @ Index(module, sourceFile, classpath, _) =>
      val requestor = sender

      val error = try {
        CompilerUtils.withCompiler(classpath) { compiler =>
          val extractor = new JarExtractor(compiler)

          logger.info(s"start indexing ${module.moduleId} (${sourceFile})")

          val entityStream = ExtractionError.logErrors(extractor(new File(sourceFile)), logger.info(_))

          searchEngine.indexEntities(module, entityStream, batchMode = true).get
          logger.info(s"${module.moduleId} has been indexed successfully")
          None
        }
      } catch {
        case e: TimeoutException =>
          logger.error(s"Indexing ${module.moduleId} timed out")
          Some(e)
        case NonFatal(e) =>
          logger.error(s"Indexing ${module.moduleId} threw $e")
          Some(e)
      }

      requestor ! Indexed(i, error)
  }
}

object Searcher {
  def props(searchEngine: SearchEngine) =
    Props(classOf[Searcher], searchEngine)
}

class Searcher(searchEngine: SearchEngine) extends Actor {
  import SearchEngineProtocol._
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

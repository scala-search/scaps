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
import scaps.webapi.Module
import scala.util.control.NonFatal
import akka.event.LoggingReceive

object SearchEngineActor {
  def props(searchEngine: SearchEngine, indexWorkerProps: Option[Props] = None) =
    Props(classOf[SearchEngineActor],
      searchEngine,
      indexWorkerProps.getOrElse(IndexWorkerActor.props(searchEngine)))
}

/**
 * Manages the state of an instance of the search engine.
 */
class SearchEngineActor(searchEngine: SearchEngine, indexWorkerProps: Props) extends Actor {
  import SearchEngineProtocol._
  import context._

  val logger = Logging(context.system, this)

  private case object Initialize
  self ! Initialize

  def receive = initialized

  def initialized: Receive = {
    val searcher = actorOf(Props(classOf[Searcher], searchEngine), "searcher")
    val indexWorker = actorOf(indexWorkerProps, "indexWorker")

    def ready: Receive = {
      val indexedModules = searchEngine.indexedModules().get
      logger.info(s"search engine ready with modules $indexedModules")

      {
        case i: Index if i.forceReindex == false =>
          enqueueJobIfNewModule(i, Nil, indexedModules)
        case i: Index =>
          enqueueJob(i, Nil, indexedModules)
        case s: Search =>
          searcher.tell(s, sender)
        case GetStatus =>
          sender ! IndexStatus(Nil, indexedModules)
        case Reset =>
          searchEngine.resetIndexes().get
          become(ready)
      }
    }

    def indexing(queue: Seq[(ActorRef, Index)], indexedModules: Seq[Module]): Receive = {
      case i: Index if i.forceReindex == false =>
        enqueueJobIfNewModule(i, queue, indexedModules)
      case i: Index =>
        enqueueJob(i, queue, indexedModules)
      case res @ Indexed(indexJob, _) if indexJob == queue.head._2 =>
        queue.head._1 ! res

        if (queue.tail.isEmpty) {
          become(ready)
        } else {
          indexWorker ! queue.tail.head._2
          become(indexing(queue.tail, indexJob.module +: indexedModules))
        }
      case Indexed(j, _) =>
        throw new IllegalStateException()
      case _: Search =>
        sender ! \/.left(s"Cannot search while index is being built. ${queue.size} modules left.")
      case GetStatus =>
        sender ! IndexStatus(queue.map(_._2.module), indexedModules)
      case Reset =>
      // TODO
    }

    def enqueueJob(indexJob: Index, queue: Seq[(ActorRef, Index)], indexedModules: Seq[Module]) = {
      if (queue.isEmpty) {
        indexWorker ! indexJob
      }

      become(indexing(queue :+ ((sender, indexJob)), indexedModules))
    }

    def enqueueJobIfNewModule(indexJob: Index, queue: Seq[(ActorRef, Index)], indexedModules: Seq[Module]) = {
      val allModules = queue.map(_._2.module) ++ indexedModules

      if (allModules.contains(indexJob.module)) {
        logger.debug(s"Drop index job $indexJob")
        sender ! Indexed(indexJob, None)
      } else {
        logger.debug(s"Enqueue unenforced index job $indexJob")
        enqueueJob(indexJob, queue, indexedModules)
      }
    }

    ready
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
    case i @ Index(module, sourceFile, classpath, _) =>
      val requestor = sender

      val error = try {
        CompilerUtils.withCompiler(classpath) { compiler =>
          val extractor = new JarExtractor(compiler)

          logger.info(s"start indexing ${module.moduleId} (${sourceFile})")

          val entityStream = ExtractionError.logErrors(extractor(new File(sourceFile)), logger.info(_))

          searchEngine.indexEntities(module, entityStream).get
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

class Searcher(searchEngine: SearchEngine) extends Actor {
  import SearchEngineProtocol._
  import scaps.searchEngine._

  def receive = {
    case Search(q, moduleIds, noResults, offset) =>
      sender ! searchEngine.search(q, moduleIds).get.map {
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
  }
}

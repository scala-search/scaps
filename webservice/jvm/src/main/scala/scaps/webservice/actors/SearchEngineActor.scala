package scaps.webservice.actors

import java.io.File
import scala.concurrent.Await
import scala.concurrent.duration._
import scaps.featureExtraction.CompilerUtils
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
import java.util.concurrent.TimeoutException
import scaps.webapi.IndexStatus
import scaps.webapi.Module

/**
 * Manages the state of an instance of the search engine.
 */
class SearchEngineActor extends Actor {
  import SearchEngineProtocol._
  import context._

  val logger = Logging(context.system, this)

  private case object Initialize
  self ! Initialize

  def receive = initializing

  def initializing: Receive = {
    case Initialize =>
      val searchEngine = SearchEngine(Settings.fromApplicationConf).get
      searchEngine.indexedModules().map { indexedModules =>
        become(initialized(searchEngine, indexedModules.toList))
      }.get
  }

  def initialized(searchEngine: SearchEngine, indexedModules: List[Module]): Receive = {
    val searcher = actorOf(Props(classOf[Searcher], searchEngine), "searcher")
    val indexWorker = actorOf(Props(classOf[IndexWorkerActor], searchEngine), "index-worker")

    def ready(indexedModules: List[Module]): Receive = {
      case i: Index if i.forceReindex == false =>
        rewriteUnenforcedIndexJob(i, indexedModules)
      case i: Index =>
        // delay first indexing job to ensure all search tasks have been completed
        system.scheduler.scheduleOnce(2.seconds) {
          searchEngine.deleteIndexes()
          indexWorker ! i
        }
        become(indexing(i :: Nil, indexedModules))
      case s: Search =>
        searcher.tell(s, sender)
      case GetStatus =>
        sender ! IndexStatus(Nil, indexedModules)
    }

    def indexing(queue: List[Index], indexedModules: List[Module]): Receive = {
      case i: Index if i.forceReindex == false =>
        rewriteUnenforcedIndexJob(i, queue.map(_.module) ++ indexedModules)
      case i: Index =>
        become(indexing(queue :+ i, indexedModules))
      case Indexed(j, _) if j == queue.head =>
        if (queue.tail.isEmpty) {
          searchEngine.indexedModules().map { indexedModules =>
            become(ready(indexedModules.toList))
          }.get
        } else {
          indexWorker ! queue.tail.head
          become(indexing(queue.tail, j.module :: indexedModules))
        }
      case Indexed(j, _) =>
        throw new IllegalStateException()
      case Search(_) =>
        sender ! \/.left(s"Cannot search while index is being built. ${queue.size} modules left.")
      case GetStatus =>
        sender ! IndexStatus(queue.map(_.module), indexedModules)
    }

    ready(indexedModules)
  }

  def rewriteUnenforcedIndexJob(i: Index, indexedModules: List[Module]) =
    if (i.forceReindex)
      throw new IllegalArgumentException
    else {
      if (!indexedModules.contains(i.module)) {
        logger.debug(s"Rewrite index job $i with forceReindex=true")
        self.tell(i.copy(forceReindex = true), sender)
      } else {
        logger.debug(s"Drop index job $i")
      }
    }
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

      CompilerUtils.withCompiler(classpath.toList) { compiler =>
        val extractor = new JarExtractor(compiler)

        logger.info(s"start indexing ${module.moduleId} (${sourceFile})")

        val f = searchEngine.indexEntities(module, extractor(new File(sourceFile)))

        val error = try {
          Await.ready(f, 1.hour)
          logger.info(s"${module.moduleId} has been indexed successfully")
          None
        } catch {
          case e: TimeoutException =>
            logger.error(s"Indexing ${module.moduleId} timed out")
            Some(e)
        }

        requestor ! Indexed(i, error)
      }
  }
}

class Searcher(searchEngine: SearchEngine) extends Actor {
  import SearchEngineProtocol._
  import scaps.searchEngine._

  def receive = {
    case Search(q) =>
      sender ! searchEngine.search(q).get.map {
        case terms => terms.take(10)
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

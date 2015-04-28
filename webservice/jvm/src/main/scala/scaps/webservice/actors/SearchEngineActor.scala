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
      become(initialized(searchEngine))
  }

  def initialized(searchEngine: SearchEngine): Receive = {
    val searcher = actorOf(Props(classOf[Searcher], searchEngine), "searcher")
    val indexWorker = actorOf(Props(classOf[IndexWorkerActor], searchEngine), "index-worker")

    def ready(): Receive = {
      case i: Index =>
        // delay first indexing job to ensure all search tasks have been completed
        system.scheduler.scheduleOnce(2.seconds) {
          searchEngine.deleteIndexes()
          indexWorker ! i
        }
        become(indexing(i :: Nil))
      case s: Search =>
        searcher.tell(s, sender)
      case GetStatus =>
        searchEngine.indexedModules().foreach { modules =>
          sender ! IndexStatus(Nil, modules)
        }
    }

    def indexing(queue: List[Index]): Receive = {
      case i: Index =>
        become(indexing(queue :+ i))
      case Indexed(j, _) if j == queue.head =>
        if (queue.tail.isEmpty) {
          become(ready())
        } else {
          indexWorker ! queue.tail.head
          become(indexing(queue.tail))
        }
      case Indexed(j, _) =>
        throw new IllegalStateException()
      case Search(_) =>
        sender ! \/.left(s"Cannot search while index is being built. ${queue.size} modules left.")
      case GetStatus =>
        searchEngine.indexedModules().foreach { modules =>
          sender ! IndexStatus(queue.map(_.module), modules)
        }
    }

    ready()
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
    case i @ Index(module, sourceFile, classpath) =>
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

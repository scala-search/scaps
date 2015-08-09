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
import scaps.webapi.IndexJob
import scaps.webapi.IndexBusy

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
        case i @ Index(jobs, _) =>
          indexWorker ! i
          sender ! true
          become(indexing(indexedModules, jobs, indexErrors))
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

    def indexing(indexedModules: Set[Module], jobs: Seq[IndexJob], indexErrors: Seq[String]): Receive = {
      case Indexed(finishedJobs, None) =>
        become(ready(indexedModules ++ finishedJobs.map(_.module), indexErrors))
      case Indexed(_, Some(error)) =>
        become(ready(indexedModules, indexErrors :+ error.toString()))
      case _: Index =>
        sender ! false
      case _: Search =>
        sender ! \/.left(s"Cannot search while index is being built.")
      case GetStatus =>
        sender ! IndexBusy(jobs.map(_.module), indexedModules.toSeq, indexErrors)
      case Reset =>
        become(resetting)
    }

    def resetting: Receive = {
      case _: Indexed =>
        searchEngine.resetIndexes().get
        become(ready(Set(), Nil))
      case _: Index =>
        sender ! false
      case _: Search =>
        sender ! \/.left(s"Cannot search while index is resetting.")
      case GetStatus =>
        sender ! IndexBusy(Nil, Nil, Nil)
      case Reset =>
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

      requestor ! Indexed(jobs, error)
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

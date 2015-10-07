package scaps.webservice.actors

import java.io.File
import scala.util.control.NonFatal
import ActorProtocol._
import akka.actor.Actor
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.event.Logging
import scalaz.std.list._
import scalaz.\/
import scaps.api.IndexBusy
import scaps.api.IndexReady
import scaps.api.IndexStatus
import scaps.api.ValueDef
import scaps.searchEngine.NameAmbiguous
import scaps.searchEngine.NameNotFound
import scaps.searchEngine.SearchEngine
import scaps.searchEngine.SyntaxError
import scaps.searchEngine.TooUnspecific
import scaps.searchEngine.UnexpectedNumberOfTypeArgs
import scaps.settings.Settings
import scaps.utils.TraversableOps
import scaps.api.Definition
import akka.actor.ActorRef
import scala.io.Source
import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.JavaConverters._
import scala.util.Try
import scaps.api.IndexEmpty
import scaps.api.IndexBusy
import scaps.api.Module

object Director {
  def props(settings: Settings)(
    managerProps: Settings => Props = SearchEngineManager.props _) =
    Props(classOf[Director], settings, managerProps)
}

class Director(baseSettings: Settings, managerProps: Settings => Props) extends Actor {
  import context._

  val logger = Logging(system, this)

  val activeIndexFilePath = Paths.get(s"${baseSettings.index.indexDir}/activeIndex")

  def mkManager(indexName: String) = {
    val settings = baseSettings.modIndex { index =>
      index.copy(indexDir = s"${index.indexDir}/$indexName")
    }

    actorOf(managerProps(settings))
  }

  def receive = {

    def ready(managers: Map[String, ActorRef], activeManager: Option[ActorRef]): Receive = {
      case GetStatus =>
        activeManager.fold {
          sender ! IndexEmpty
        } { mngr =>
          mngr.tell(GetStatus, sender)
        }
      case s: Search =>
        activeManager.fold {
          sender ! \/.left("No active index found.")
        } { mngr =>
          mngr.tell(s, sender)
        }
      case i @ Index(indexName, _) =>
        val manager = managers.get(indexName).getOrElse {
          mkManager(indexName)
        }

        manager ! i
        become(ready(managers + (indexName -> manager), activeManager))
      case f @ FinalizeIndex(indexName) =>
        managers.get(indexName).foreach { manager =>
          manager ! f
        }
      case Finalized(indexName) =>
        logger.debug(s"new active index: $sender ($indexName), previously: $activeManager")
        Files.write(activeIndexFilePath, indexName.getBytes())
        become(ready(managers, Some(sender)))
    }

    val activeManager = for {
      lines <- Try { Files.readAllLines(activeIndexFilePath).asScala }.toOption
      activeIndexName <- lines.headOption
    } yield mkManager(activeIndexName)

    ready(Map(), activeManager)
  }
}

object SearchEngineManager {
  def props(settings: Settings) =
    Props(classOf[SearchEngineManager], settings, Searcher.props _)
}

class SearchEngineManager(settings: Settings, searcherProps: SearchEngine => Props) extends Actor {
  import context._

  val logger = Logging(system, this)

  def receive = {
    val engine = SearchEngine(settings).get

    def indexing(): Receive = {
      case GetStatus =>
        sender ! IndexBusy(Nil, Nil)
      case Index(_, defs) =>
        engine.index(defs).get
      case FinalizeIndex(name) =>
        engine.finalizeIndex().get
        sender ! Finalized(name)
        become(ready(engine.indexedModules().get))
    }
    def ready(indexedModules: Seq[Module]): Receive = {
      case GetStatus =>
        sender ! IndexReady(indexedModules, Nil)
      case s: Search =>
        val searcher = actorOf(searcherProps(engine))
        searcher.tell(s, sender)
      case i: Index =>
        self ! i
        become(indexing())
    }

    ready(engine.indexedModules().get)
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
      try {
        val res = searchEngine.search(q, moduleIds).get.map {
          case values => values.drop(offset).take(noResults)
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
      } catch {
        case MaximumClauseCountExceededException =>
          sender ! \/.left("Query type too complex")
      } finally {
        context.stop(self)
      }
  }
}

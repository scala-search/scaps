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
import scaps.searchEngine.SystemError

case class Index(sourceFile: String, classpath: Seq[String])
case class Search(query: String)

case object GetQueue
case class Done(job: Index)

object SearchEngineActor {
  sealed trait State
  case object Idle extends State
  case object Active extends State
}

class SearchEngineActor extends FSM[SearchEngineActor.State, List[Index]] {
  import SearchEngineActor._

  val logger = Logging(context.system, this)

  val searchEngine = SearchEngine(Settings.fromApplicationConf).get
  val worker = context.actorOf(Props(classOf[IndexWorkerActor], searchEngine), "indexWorker")

  startWith(Idle, Nil)

  when(Idle) {
    case Event(i: Index, Nil) =>
      worker ! i
      goto(Active) using (i :: Nil)
    case Event(Search(query), Nil) =>
      sender ! searchEngine.search(query)
      stay
  }

  when(Active) {
    case Event(i: Index, queue) =>
      logger.info(s"defer processing of ${i.sourceFile}")
      stay using (queue :+ i)
    case Event(Done(j), head :: Nil) if j == head =>
      goto(Idle) using (Nil)
    case Event(Done(j), head :: queue) if j == head =>
      worker ! queue.head
      stay using (queue)
    case Event(Search, queue) =>
      sender ! \/.left(SystemError(s"Cannot search while index is being built. ${queue.size} documents left"))
      stay
  }

  whenUnhandled {
    case Event(GetQueue, queue) =>
      sender ! queue.map(_.sourceFile)
      stay
  }

  initialize()
}

class IndexWorkerActor(searchEngine: SearchEngine) extends Actor {
  import scala.concurrent.ExecutionContext.Implicits.global

  val logger = Logging(context.system, this)

  def receive = {
    case i @ Index(sourceFile, classpath) =>
      val requestor = sender

      CompilerUtils.withCompiler(classpath.toList) { compiler =>
        val extractor = new JarExtractor(compiler)

        logger.info(s"start indexing ${sourceFile}")

        val f = searchEngine.indexEntities(extractor(new File(sourceFile)))

        Await.ready(f, 1.hour)

        logger.info(s"${sourceFile} has been indexed successfully")
        requestor ! Done(i)
      }
  }
}

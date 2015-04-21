package scala.tools.apiSearch.webservice

import java.io.File
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.tools.apiSearch.featureExtraction.CompilerUtils
import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.tools.apiSearch.searchEngine.SearchEngine
import scala.tools.apiSearch.settings.Settings
import scala.tools.nsc.interactive.Global
import scala.util.Try
import akka.actor.Actor
import akka.actor.FSM
import akka.actor.Props
import akka.event.Logging

case class Index(filePath: String, classpath: List[String])
case object GetQueue
case object Done

sealed trait State
case object Idle extends State
case object Active extends State

class IndexActor extends FSM[State, List[Index]] {
  val logger = Logging(context.system, this)
  val worker = context.actorOf(Props[IndexWorkerActor])

  startWith(Idle, Nil)

  when(Idle) {
    case Event(i: Index, Nil) =>
      worker ! i
      goto(Active)
  }

  when(Active) {
    case Event(i: Index, queue) =>
      logger.info(s"defer processing of ${i.filePath}")
      stay using (queue :+ i)
    case Event(Done, Nil) =>
      goto(Idle)
    case Event(Done, queue) =>
      worker ! queue.head
      stay using (queue.tail)
  }

  whenUnhandled {
    case Event(GetQueue, queue) =>
      sender ! queue
      stay
  }
}

class IndexWorkerActor extends Actor {
  val logger = Logging(context.system, this)

  def receive = {
    case Index(file, classpath) =>
      Try {
        CompilerUtils.withCompiler(classpath) { compiler =>
          val extractor = new JarExtractor(compiler)
          val searchEngine = SearchEngine(Settings.fromApplicationConf).get

          logger.info(s"start indexing $file")

          Await.result(searchEngine.indexEntities(extractor(new File(file))), 1.hour)

          logger.info(s"$file has been indexed")
        }
      }
      sender ! Done
  }
}

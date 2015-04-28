package scaps.webservice

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import akka.actor.ActorRefFactory
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import akka.util.Timeout
import scalaz.{ \/ => \/ }
import scaps.webapi.IndexStatus
import scaps.webapi.Module
import scaps.webapi.ScapsApi
import scaps.webapi.ScapsControlApi
import scaps.webapi.TermEntity
import scaps.webservice.actors.SearchEngineActor
import scaps.webservice.actors.SearchEngineProtocol.Index
import scaps.webservice.actors.SearchEngineProtocol.GetStatus
import scaps.webservice.actors.SearchEngineProtocol.Search

class Scaps(context: ActorRefFactory) extends ScapsApi with ScapsControlApi {
  val searchEngine = context.actorOf(Props[SearchEngineActor], "searchEngine")

  implicit val _ = context.dispatcher
  implicit val timeout = Timeout(10.seconds)

  def index(module: Module, artifactPath: String, classpath: Seq[String]): Unit = {
    searchEngine ! Index(module, artifactPath, classpath)
  }

  def getStatus(): Future[IndexStatus] =
    (searchEngine ? GetStatus).mapTo[IndexStatus]

  def search(query: String): Future[Either[String, Seq[TermEntity]]] =
    for {
      result <- (searchEngine ? Search(query)).mapTo[String \/ Seq[TermEntity]]
    } yield result.toEither
}

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

class Scaps(context: ActorRefFactory) extends ScapsApi with ScapsControlApi {
  import scaps.webservice.actors.SearchEngineProtocol._

  val searchEngine = context.actorOf(Props[SearchEngineActor], "searchEngine")

  implicit val _ = context.dispatcher
  implicit val timeout = Timeout(10.seconds)

  def index(module: Module, artifactPath: String, classpath: Seq[String], forceReindex: Boolean): Unit = {
    searchEngine ! Index(module, artifactPath, classpath, forceReindex)
  }

  def resetIndexes(): Unit = {
    searchEngine ! Reset
  }

  def getStatus(): Future[IndexStatus] =
    (searchEngine ? GetStatus).mapTo[IndexStatus]

  def search(query: String, noResults: Int, offset: Int): Future[Either[String, Seq[TermEntity]]] =
    for {
      result <- (searchEngine ? Search(query, noResults, offset)).mapTo[String \/ Seq[TermEntity]]
    } yield result.toEither
}

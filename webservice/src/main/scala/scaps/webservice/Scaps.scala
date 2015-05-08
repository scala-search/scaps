package scaps.webservice

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import akka.actor.ActorRefFactory
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import akka.util.Timeout
import scalaz.{ \/ => \/ }
import scaps.searchEngine.SearchEngine
import scaps.settings.Settings
import scaps.webapi.IndexStatus
import scaps.webapi.Module
import scaps.webapi.ScapsApi
import scaps.webapi.ScapsControlApi
import scaps.webapi.TermEntity
import scaps.webservice.actors.SearchEngineActor
import scaps.webservice.actors.UserInteractionLogger

class Scaps(context: ActorRefFactory) extends ScapsApi with ScapsControlApi {
  import scaps.webservice.actors.SearchEngineProtocol._

  val searchEngine = {
    val se = SearchEngine(Settings.fromApplicationConf).get
    context.actorOf(SearchEngineActor.props(se)(), "searcher")
  }
  val userInteractionLogger = context.actorOf(Props[UserInteractionLogger], "userInteractionLogger")

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

  def search(query: String, moduleIds: Set[String], noResults: Int, offset: Int): Future[Either[String, Seq[TermEntity]]] = {
    val searchMsg = Search(query, moduleIds, noResults, offset)
    for {
      result <- (searchEngine ? searchMsg).mapTo[String \/ Seq[TermEntity]]
    } yield {
      userInteractionLogger ! ((searchMsg, result))
      result.toEither
    }
  }

  def assessPositivley(query: String, moduleIds: Set[String], resultNo: Int, termSignature: String): Unit =
    userInteractionLogger ! PositiveAssessement(query, moduleIds, resultNo, termSignature)
}

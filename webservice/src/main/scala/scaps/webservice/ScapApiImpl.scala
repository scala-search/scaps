package scaps.webservice

import akka.actor.ActorSystem
import scaps.webapi.ScapsApi
import akka.actor.Props
import scaps.webapi.IndexStatus
import scala.concurrent.Future
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

class ScapsApiImpl(system: ActorSystem) extends ScapsApi {
  val indexActor = system.actorOf(Props[IndexActor])

  implicit val _ = system.dispatcher
  implicit val timeout = Timeout(10.seconds)

  def index(sourceFile: String, classpath: Seq[String]): Unit = {
    indexActor ! Index(sourceFile, classpath)
  }

  def getStatus(): Future[IndexStatus] =
    for {
      queue <- (indexActor ? GetQueue).mapTo[List[String]]
    } yield IndexStatus(queue)
}

package scaps.webservice

import scala.concurrent.duration.DurationInt
import akka.actor.ActorSystem
import akka.util.Timeout
import scaps.api.ScapsApi
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.routing.Directive.pimpApply
import spray.routing.SimpleRoutingApp
import upickle._
import spray.http.StatusCodes
import spray.http.HttpEntity
import spray.http.MediaTypes
import scalatags.Text
import akka.actor.Props
import akka.io.IO
import spray.can.Http
import akka.actor.Actor
import akka.pattern.ask
import akka.actor.ActorRef
import akka.actor.Terminated

object Main extends App {
  val settings = WebserviceSettings.fromApplicationConf

  implicit val system = ActorSystem("scapsSystem")
  implicit val executionContext = system.dispatcher
  implicit val timeout = Timeout(1.second)

  val scaps = new Scaps(system)

  val service = system.actorOf(Props(classOf[ScapsServiceActor], scaps), "scapsService")
  val controlService = system.actorOf(Props(classOf[ScapsControlServiceActor], scaps), "scapsControlService")

  IO(Http) ! Http.Bind(service, settings.interface, port = settings.port)
  IO(Http) ! Http.Bind(controlService, settings.controlInterface, port = settings.controlPort)
}

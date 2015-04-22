package scaps.webservice

import scala.concurrent.duration.DurationInt

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import akka.util.Timeout
import spray.http.StatusCodes
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.routing.Directive.pimpApply
import spray.routing.SimpleRoutingApp

object Main extends App with SimpleRoutingApp {
  implicit val system = ActorSystem("api-search-system")
  implicit val _ = system.dispatcher
  implicit val timeout = Timeout(10.seconds)
  val indexActor = system.actorOf(Props[IndexActor])

  startServer(interface = "localhost", port = 8080) {
    path("index") {
      post {
        extract(_.request.entity.asString) { e =>
          indexActor ! upickle.read[Index](e)
          complete(StatusCodes.Accepted)
        }
      }
    } ~
      path("status") {
        get {
          complete {
            (indexActor ? GetQueue)
              .mapTo[List[Index]]
              .map {
                case Nil   => <p>Queue is empty</p>
                case queue => <ul>${ queue.map(entry => <li>${ entry.sourceFile }</li>) }</ul>
              }
          }
        }
      }
  }
}

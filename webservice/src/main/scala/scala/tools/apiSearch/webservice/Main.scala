package scala.tools.apiSearch.webservice

import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout

import spray.http.StatusCodes
import spray.routing.SimpleRoutingApp

import upickle._

object Main extends App with SimpleRoutingApp {
  implicit val system = ActorSystem("api-search-system")
  implicit val _ = system.dispatcher
  implicit val timeout = Timeout(10.seconds)
  val indexActor = system.actorOf(Props[IndexActor])

  startServer(interface = "localhost", port = 8080) {
    path("index") {
      post {
        extract(_.request.entity.asString) { e =>
          //val i = upickle.read[List[Blub]](e)
          indexActor ! e
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

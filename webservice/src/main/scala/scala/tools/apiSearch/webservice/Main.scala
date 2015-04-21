package scala.tools.apiSearch.webservice

import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout

import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.routing.SimpleRoutingApp

object Protocol extends DefaultJsonProtocol {
  implicit val IndexFormat = jsonFormat2(Index)
}

object Main extends App with SimpleRoutingApp with SprayJsonSupport {
  import Protocol._

  implicit val system = ActorSystem("api-search-system")
  implicit val _ = system.dispatcher
  implicit val timeout = Timeout(10.seconds)
  val indexActor = system.actorOf(Props[IndexActor])

  startServer(interface = "localhost", port = 8080) {
    path("index") {
      post {
        entity(as[Index]) { i =>
          indexActor ! i
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
                case queue => <ul>${ queue.map(entry => <li>${ entry.filePath }</li>) }</ul>
              }
          }
        }
      }
  }
}

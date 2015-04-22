package scaps.webservice

import scala.concurrent.duration.DurationInt
import akka.actor.ActorSystem
import akka.util.Timeout
import scaps.webapi.ScapsApi
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.routing.Directive.pimpApply
import spray.routing.SimpleRoutingApp
import upickle._
import spray.http.StatusCodes

object Main extends App with SimpleRoutingApp {
  implicit val system = ActorSystem("api-search-system")
  implicit val _ = system.dispatcher
  implicit val timeout = Timeout(10.seconds)

  val apiImpl = new ScapsApiImpl(system)

  startServer(interface = "localhost", port = 8080) {
    pathSingleSlash {
      get {
        complete { StatusCodes.Accepted }
      }
    } ~
      path("api" / Segments) { path =>
        post {
          extract(_.request.entity.asString) { e =>
            complete {
              Router.route[ScapsApi](apiImpl)(
                autowire.Core.Request(path, upickle.read[Map[String, String]](e)))
            }
          }
        }
      }
  }
}

object Router extends autowire.Server[String, upickle.Reader, upickle.Writer] {
  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

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
import spray.http.HttpEntity
import spray.http.MediaTypes
import scalatags.Text

object Main extends App with SimpleRoutingApp {
  implicit val system = ActorSystem("api-search-system")
  implicit val _ = system.dispatcher

  val apiImpl = new ScapsApiImpl(system)

  startServer(interface = "localhost", port = 8080) {
    pathSingleSlash {
      get {
        complete {
          for {
            status <- apiImpl.getStatus()
          } yield render(Pages.index(status))
        }
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

  def render(html: Text.TypedTag[String]): HttpEntity =
    HttpEntity(MediaTypes.`text/html`, html.render)
}

object Router extends autowire.Server[String, upickle.Reader, upickle.Writer] {
  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

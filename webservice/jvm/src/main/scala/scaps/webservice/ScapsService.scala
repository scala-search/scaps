package scaps.webservice

import spray.routing.HttpService
import akka.actor.Actor
import akka.actor.ActorSystem
import spray.http.HttpEntity
import scalatags.Text
import spray.http.MediaTypes
import spray.http.HttpEntity
import scaps.webapi.ScapsApi
import akka.io.Tcp.Bound
import scaps.webservice.ui.HtmlPages

class ScapsServiceActor extends Actor with ScapsService {
  def actorRefFactory = context

  def receive = runRoute(route)
}

trait ScapsService extends HttpService {
  implicit val _ = actorRefFactory.dispatcher

  val apiImpl = new Scaps(actorRefFactory)

  def route =
    path("api" / Segments) { path =>
      post {
        extract(_.request.entity.asString) { e =>
          complete {
            Router.route[ScapsApi](apiImpl)(
              autowire.Core.Request(path, upickle.read[Map[String, String]](e)))
          }
        }
      }
    } ~
      pathSingleSlash {
        get {
          complete {
            for {
              status <- apiImpl.getStatus()
            } yield HttpEntity(MediaTypes.`text/html`, HtmlPages.index(status).toString())
          }
        }
      } ~
      get { getFromResourceDirectory("") }
}

object Router extends autowire.Server[String, upickle.Reader, upickle.Writer] {
  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

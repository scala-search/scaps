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
import scaps.webservice.ui.Pages
import spray.http.Uri

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
          parameter('q) { query =>
            if (query.isEmpty())
              reject
            else
              complete {
                for {
                  result <- apiImpl.search(query)
                  page = HtmlPages.skeleton(result.fold(HtmlPages.queryError(_), HtmlPages.results(_)), query)
                } yield HttpEntity(MediaTypes.`text/html`, page.toString())
              }
          } ~
            complete {
              for {
                status <- apiImpl.getStatus()
                page = HtmlPages.skeleton(HtmlPages.main(status))
              } yield HttpEntity(MediaTypes.`text/html`, page.toString())
            }
        }
      } ~
      get { getFromResourceDirectory("") }
}

object HtmlPages extends Pages(scalatags.Text) {
  def encodeUri(path: String, params: Map[String, String]): String =
    (Uri(path) withQuery params).toString()
}

object Router extends autowire.Server[String, upickle.Reader, upickle.Writer] {
  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

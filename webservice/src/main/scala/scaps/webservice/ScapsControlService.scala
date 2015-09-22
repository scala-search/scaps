package scaps.webservice

import spray.routing.HttpService
import scaps.api.ScapsControlApi
import akka.actor.Actor

class ScapsControlServiceActor(val apiImpl: Scaps) extends Actor with ScapsControlService {
  def actorRefFactory = context

  def receive = runRoute(route)
}

trait ScapsControlService extends HttpService {
  implicit val _ = actorRefFactory.dispatcher

  def apiImpl: ScapsControlApi

  def route =
    path(ScapsControlApi.apiPath / Segments) { path =>
      post {
        extract(_.request.entity.asString) { e =>
          complete {
            Router.route[ScapsControlApi](apiImpl)(
              autowire.Core.Request(path, upickle.read[Map[String, String]](e)))
          }
        }
      }
    }
}

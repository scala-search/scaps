/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

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
              autowire.Core.Request(path, upickle.default.read[Map[String, String]](e)))
          }
        }
      }
    }
}

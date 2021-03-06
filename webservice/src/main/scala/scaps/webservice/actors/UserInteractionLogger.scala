/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.webservice.actors

import akka.actor.Actor
import scalaz._
import scaps.api.ValueDef
import akka.event.Logging

class UserInteractionLogger extends Actor {
  import ActorProtocol._

  val logger = Logging(context.system, this)

  def receive = {
    case (Search(query, moduleIds, _, _), -\/(error: String)) =>
      logger.info(s"failed; $query; $moduleIds; 0; $error;")
    case (Search(query, moduleIds, noResults, offset), \/-(_)) =>
      val page = offset / noResults
      logger.info(s"succeeded; $query; $moduleIds; $page;")
    case PositiveAssessement(query, moduleIds, signature) =>
      logger.info(s"plusOne; $query; $moduleIds; $signature;")
  }
}

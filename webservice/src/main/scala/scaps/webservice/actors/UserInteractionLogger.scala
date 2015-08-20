package scaps.webservice.actors

import akka.actor.Actor
import scalaz._
import scaps.webapi.ValueDef
import akka.event.Logging

class UserInteractionLogger extends Actor {
  import ActorProtocol._

  val logger = Logging(context.system, this)

  def receive = {
    case (Search(query, moduleIds, _, _), -\/(error: String)) if error.contains("not found") =>
      logger.info(s"failed; $query; $moduleIds; 0; $error;")
    case (Search(query, moduleIds, noResults, offset), \/-(_)) =>
      val page = offset / noResults
      logger.info(s"succeeded; $query; $moduleIds; $page;")
    case PositiveAssessement(query, resultNo, moduleIds, signature) =>
      logger.info(s"plusOne; $query; $moduleIds; $resultNo; $signature;")
  }
}

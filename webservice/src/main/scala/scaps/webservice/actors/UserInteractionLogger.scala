package scaps.webservice.actors

import akka.actor.Actor
import scalaz._
import scaps.webapi.TermEntity
import akka.event.Logging

class UserInteractionLogger extends Actor {
  import SearchEngineProtocol._

  val logger = Logging(context.system, this)

  def receive = {
    case (Search(query, _, _), -\/(error: String)) if error.contains("not found") =>
      logger.info(s"failed; $query; 0; $error;")
    case (Search(query, noResults, offset), _) =>
      val page = offset / noResults
      logger.info(s"succeeded; $query; $page;")
    case PositiveAssessement(query, resultNo, signature) =>
      logger.info(s"plusOne; $query; $resultNo; $signature;")
  }
}

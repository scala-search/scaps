package scaps.webapi

import scala.concurrent.Future

case class IndexStatus(workQueue: Seq[String])

/**
 * The main API exposed to search engine users.
 *
 * All exposed methods are considered safe and should not lead to erroneous behavior
 * when missused.
 */
trait ScapsApi extends CommonApi {
  def search(query: String): Future[Either[String, Seq[TermEntity]]]
}

/**
 * A control API which access may be limited to local requests.
 *
 * The exposed methods may destroy the index or will take a long time to process.
 */
trait ScapsControlApi extends CommonApi {
  def index(artifactPath: String, classpath: Seq[String]): Unit
}

/**
 * Methods exposed by both APIs.
 */
trait CommonApi {
  def getStatus(): Future[IndexStatus]
}
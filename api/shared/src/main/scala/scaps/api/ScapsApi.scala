package scaps.api

import scala.concurrent.Future

sealed trait IndexStatus {
  def workQueue: Seq[Module]
  def indexedModules: Seq[Module]
  def indexErrors: Seq[String]
  def isReady: Boolean

  def allModules = workQueue ++ indexedModules
}
case class IndexReady(indexedModules: Seq[Module], indexErrors: Seq[String]) extends IndexStatus {
  val workQueue = Nil
  val isReady = true
}
case class IndexBusy(workQueue: Seq[Module], indexedModules: Seq[Module], indexErrors: Seq[String]) extends IndexStatus {
  val isReady = false
}

case class IndexJob(module: Module, artifactPath: String, docUrlPrefix: Option[String])

/**
 * The main API exposed to search engine users.
 *
 * All exposed methods are considered safe and should not lead to erroneous behavior
 * when missused.
 */
trait ScapsApi extends CommonApi {
  def search(
    query: String,
    moduleIds: Set[String] = Set(),
    noResults: Int = ScapsApi.defaultPageSize,
    offset: Int = 0): Future[Either[String, Seq[ValueDef]]]

  def assessPositivley(query: String, moduleIds: Set[String], resultNo: Int, valueSignature: String): Unit
}

object ScapsApi {
  val apiPath = "api"

  val defaultPageSize = 10
}

/**
 * A control API which access may be limited to local users.
 *
 * The exposed methods may destroy the index or will take a long time to process.
 */
trait ScapsControlApi extends CommonApi {
  /**
   * Rebuilds the index.
   */
  def index(jobs: Seq[IndexJob], classpath: Seq[String]): Future[Boolean]
}

object ScapsControlApi {
  val apiPath = "api"
}

/**
 * Methods exposed by both APIs.
 */
trait CommonApi {
  def getStatus(): Future[IndexStatus]
}

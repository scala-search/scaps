package scaps.webapi

import scala.concurrent.Future

case class IndexStatus(workQueue: Seq[String])
case class SearchResult(owner: String, signature: String)

trait ScapsApi {
  def index(sourceFile: String, classpath: Seq[String]): Unit

  def getStatus(): Future[IndexStatus]

  def search(query: String): Future[Seq[SearchResult]]
}

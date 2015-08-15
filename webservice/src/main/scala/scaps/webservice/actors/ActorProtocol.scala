package scaps.webservice.actors

import scalaz.\/
import scaps.webapi.TermEntity
import scaps.webapi.Module
import scaps.webapi.IndexJob
import scaps.searchEngine.SearchEngine

object ActorProtocol {
  case class Index(jobs: Seq[IndexJob], classpath: Seq[String])
  case class Indexed(jobs: Seq[IndexJob], error: Option[Throwable], updateEngine: SearchEngine)

  case class Search(query: String, moduleIds: Set[String], noResults: Int, offset: Int)
  type Result = String \/ Seq[TermEntity]

  case class PositiveAssessement(query: String, moduleIds: Set[String], resultNo: Int, signature: String)

  case object GetStatus
}

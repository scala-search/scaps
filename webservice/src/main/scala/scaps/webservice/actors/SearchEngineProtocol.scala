package scaps.webservice.actors

import scalaz.\/
import scaps.webapi.TermEntity
import scaps.webapi.Module

object SearchEngineProtocol {
  case class Index(module: Module, sourceFile: String, classpath: Seq[String], forceReindex: Boolean)
  case class Indexed(job: Index, error: Option[Throwable])
  case object Reset

  case class Search(query: String, moduleIds: Set[String], noResults: Int, offset: Int)
  type Result = String \/ Seq[TermEntity]

  case class PositiveAssessement(query: String, moduleIds: Set[String], resultNo: Int, signature: String)

  case object GetStatus
}

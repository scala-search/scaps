package scaps.webservice.actors

import scalaz.\/
import scaps.webapi.TermEntity

object SearchEngineProtocol {
  case class Index(sourceFile: String, classpath: Seq[String])
  case class Indexed(job: Index)

  case class Search(query: String)
  type Result = String \/ Seq[TermEntity]

  case object GetQueue
}

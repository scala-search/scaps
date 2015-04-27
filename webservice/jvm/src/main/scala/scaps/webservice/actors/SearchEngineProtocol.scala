package scaps.webservice.actors

import scalaz.\/
import scaps.webapi.TermEntity
import scaps.webapi.Module

object SearchEngineProtocol {
  case class Index(module: Module, sourceFile: String, classpath: Seq[String])
  case class Indexed(job: Index)

  case class Search(query: String)
  type Result = String \/ Seq[TermEntity]

  case object GetQueue
}

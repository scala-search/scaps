package scaps.webservice.ui

import scaps.webapi.Module

case class PageState(query: Variable[String], moduleId: Variable[Option[String]])

object PageState {
  val empty = PageState(Variable(""), Variable(None))
}

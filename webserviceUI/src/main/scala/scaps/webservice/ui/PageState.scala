package scaps.webservice.ui

import scaps.webapi.Module

case class PageState(query: Variable[String], moduleIds: Variable[Set[String]])

object PageState {
  val empty = PageState(Variable(""), Variable(Set()))
}

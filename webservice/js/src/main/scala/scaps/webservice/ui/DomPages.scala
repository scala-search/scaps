package scaps.webservice.ui

import scalatags.JsDom.all._
import scaps.webapi.IndexStatus
import scaps.webapi.SearchResult
import scala.scalajs.js

object DomPages extends Pages(scalatags.JsDom) {
  def encodeUri(path: String, params: Map[String, String]): String = {
    def encode(s: String) = js.Dynamic.global.encodeURIComponent(s).asInstanceOf[String]

    params.map {
      case (key, value) => s"${encode(key)}=${encode(value)}"
    }.mkString("?", "&", "")
  }

  def connectionError() =
    div(cls := "alert alert-error")("The server is currently unreachable. Please try again later.")
}

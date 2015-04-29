package scaps.webservice.ui

import scalatags.JsDom.all._
import scaps.webapi.IndexStatus
import scala.scalajs.js

object DomPages extends Pages(scalatags.JsDom) {
  def encodeUri(path: String, params: Map[String, Any]): String = {
    def encode(s: String) = js.Dynamic.global.encodeURIComponent(s).asInstanceOf[String]

    params.map {
      case (key, value) => s"${encode(key)}=${encode(value.toString())}"
    }.mkString("?", "&", "")
  }
}

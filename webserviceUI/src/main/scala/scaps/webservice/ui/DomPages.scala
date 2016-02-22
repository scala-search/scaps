/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.webservice.ui

import scalatags.JsDom.all._
import scaps.api.IndexStatus
import scala.scalajs.js
import org.scalajs.dom

object DomPages extends Pages(scalatags.JsDom) {
  def encodeUri(path: String, params: List[(String, String)]): String = {
    def encode(s: String) = js.Dynamic.global.encodeURIComponent(s).asInstanceOf[String]

    params.map {
      case (key, value) => s"${encode(key)}=${encode(value)}"
    }.mkString("?", "&", "")
  }
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.webservice.ui

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import org.scalajs.dom

class AjaxClient(apiPath: String) extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
  override def doCall(req: Request) = {
    val path = s"/$apiPath/${req.path.mkString("/")}"
    val pickled = upickle.default.write(req.args)
    val xmlReq =
      if (req.path.last == "search") {
        val encoded = js.Dynamic.global.encodeURIComponent(pickled).asInstanceOf[String]
        dom.ext.Ajax.get(
          url = s"$path?data=$encoded")
      } else {
        dom.ext.Ajax.post(
          url = path,
          data = pickled)
      }

    xmlReq.map(_.responseText)
  }

  def read[Result: upickle.default.Reader](p: String) = upickle.default.read[Result](p)
  def write[Result: upickle.default.Writer](r: Result) = upickle.default.write(r)
}

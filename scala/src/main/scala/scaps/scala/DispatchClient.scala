/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.scala

import dispatch._
import dispatch.Defaults._

class DispatchClient(hostName: String, apiPath: String) extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
  val h = new Http

  override def doCall(req: Request): Future[String] = {
    val service = host(hostName)

    val path = req.path.foldLeft(service / apiPath)(_ / _)

    val request = path.POST
      .setContentType("application/json", "UTF-8")
      .<<(upickle.default.write(req.args))

    h(request).map(_.getResponseBody)
  }

  def read[Result: upickle.default.Reader](p: String) = upickle.default.read[Result](p)
  def write[Result: upickle.default.Writer](r: Result) = upickle.default.write(r)
}

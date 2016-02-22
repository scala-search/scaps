/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.scala

import dispatch._
import dispatch.Defaults._

class DispatchClient(hostName: String, apiPath: String) extends autowire.Client[String, upickle.Reader, upickle.Writer] {
  val h = new Http

  override def doCall(req: Request): Future[String] = {
    val service = host(hostName)

    val path = req.path.foldLeft(service / apiPath)(_ / _)

    val request = path.POST
      .setContentType("application/json", "UTF-8")
      .<<(upickle.write(req.args))

    h(request).map(_.getResponseBody)
  }

  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.api

case class Module(organization: String, name: String, revision: String) {
  def moduleId = s"$organization:$name:$revision"
  def isSnapshot = revision.endsWith("SNAPSHOT")
}

object Module {
  val Unknown = Module("unknown", "unknown", "0.1.0-SNAPSHOT")
}

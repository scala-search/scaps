/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.utils

import scala.util.Try
import scala.language.reflectiveCalls

object using {
  def apply[R <: { def close(): Unit }, T](resource: => R)(f: R => T): Try[T] = {
    Try {
      val r = resource

      val res = Try {
        f(r)
      }

      r.close()

      res.get
    }
  }
}

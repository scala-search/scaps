/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.utils

import scala.concurrent.duration._

object timers {
  def withTime[R](block: => R): (R, Duration) = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    (result, (end - start).nanos)
  }

  def printTime[R](desc: String)(block: => R): R = {
    val (res, duration) = withTime(block)
    println(s"Elapsed time for $desc: ${duration.toMillis} ms")
    res
  }
}

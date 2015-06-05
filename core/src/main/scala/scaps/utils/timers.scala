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
    println(s"Elapsed time for $desc: ${duration.toMicros} µ")
    res
  }
}

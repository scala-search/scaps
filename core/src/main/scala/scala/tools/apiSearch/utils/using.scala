package scala.tools.apiSearch.utils

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

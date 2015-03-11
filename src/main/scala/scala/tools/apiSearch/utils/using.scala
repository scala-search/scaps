package scala.tools.apiSearch.utils

import scala.util.Try
import scala.language.reflectiveCalls

object using {
  def apply[R <: { def close(): Unit }, T](resource: R)(f: R => T): T = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }
}

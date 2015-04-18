package scala.tools.apiSearch.utils

object printval {
  def apply[A](desc: String, v: A): A = {
    println(s"$desc: $v")
    v
  }
}

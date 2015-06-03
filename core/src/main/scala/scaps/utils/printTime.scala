package scaps.utils

object printTime {
  def apply[R](desc: String)(block: => R): R = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    println(s"Elapsed time for $desc: ${(end - start)} ns")
    result
  }
}

package scaps.webapi

trait ScapsApi {
  def index(sourceFile: String, classpath: Seq[String]): Unit
}

package scaps.api

case class Module(organization: String, name: String, revision: String) {
  def moduleId = s"$organization:$name:$revision"
  def isSnapshot = revision.endsWith("SNAPSHOT")
}

object Module {
  val Unknown = Module("unknown", "unknown", "0.1.0-SNAPSHOT")
}

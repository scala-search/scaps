package scaps.api

case class DocComment(body: String, attributes: Map[String, String]) {
  lazy val indexableContent: String =
    body.replaceAll("""<(.|\n)+?>""", "") + attributes.values.mkString("\n", "\n", "")
}

object DocComment {
  val empty = DocComment("", Map())
}

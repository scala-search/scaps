package scaps.api

case class DocComment(body: String, attributes: List[(String, String)]) {
  lazy val indexableContent: String =
    DocComment.stripHtml(body) +
      attributes.map(_._2).map(DocComment.stripHtml).mkString("\n", "\n", "")
}

object DocComment {
  val empty = DocComment("", List())

  def stripHtml(text: String) =
    text.replaceAll("""<(.|\n)+?>""", "")
}

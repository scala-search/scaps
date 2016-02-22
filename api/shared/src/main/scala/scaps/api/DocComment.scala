/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

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

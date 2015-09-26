package scaps.featureExtraction

import scala.tools.nsc.Global
import scala.tools.nsc.doc.Settings
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.doc.model.CommentFactory
import scala.tools.nsc.doc.model.MemberLookup
import scala.tools.nsc.doc.model.ModelFactory
import scala.tools.nsc.doc.model.ModelFactoryImplicitSupport
import scala.tools.nsc.doc.model.ModelFactoryTypeSupport
import scala.tools.nsc.doc.model.TreeFactory
import scala.tools.nsc.doc.model.diagram.DiagramFactory

import scalaz.std.boolean.option
import scaps.api.DocComment

class ScalaDocParser(compiler: Global, settings: Settings) {
  def apply(comment: String): DocComment =
    scaladoc.parse(comment)

  private object scaladoc extends ModelFactory(compiler, compiler.settings.asInstanceOf[Settings]) with ModelFactoryImplicitSupport with ModelFactoryTypeSupport with DiagramFactory with CommentFactory with TreeFactory with MemberLookup {
    import scala.tools.nsc.doc.base.comment._

    def parse(comment: String): DocComment =
      toScaps(parseAtSymbol(comment, "", compiler.NoPosition))

    def toScaps(comment: Comment): DocComment = {
      val attrs = List(
        toAttr("typeParams", comment.typeParams.toMap),
        toAttr("params", comment.valueParams.toMap),
        toAttr("returns", comment.result),
        toAttr("throws", comment.throws.toMap),
        toAttr("note", comment.note),
        toAttr("see", comment.see),
        toAttr("example", comment.example),
        toAttr("authors", comment.authors),
        toAttr("todo", comment.todo))

      DocComment(toHtmlString(comment.body), attrs.flatten)
    }

    def toAttr(name: String, value: List[Body]): Option[(String, String)] =
      value match {
        case Nil      => None
        case v :: Nil => Some((name, toHtmlString(v)))
        case vs       => Some((name, tag("ul", vs.map(v => tag("li", toHtmlString(v))).mkString)))
      }

    def toAttr(name: String, value: Option[Body]): Option[(String, String)] =
      value.map(v => (name, toHtmlString(v)))

    def toAttr(name: String, value: Map[String, Body]): Option[(String, String)] =
      option(!value.isEmpty, (name, tag("dl",
        value.map { case (k, v) => tag("dt", k) + tag("dd", toHtmlString(v)) }.mkString)))

    def toHtmlString(body: Body): String = {
      body.blocks.map(toHtmlString).mkString
    }

    def toHtmlString(block: Block): String = block match {
      case Title(text, level) =>
        tag(s"h${level + 2}", toHtmlString(text))
      case Paragraph(text) =>
        tag("p", toHtmlString(text))
      case Code(data) =>
        tag("pre", tag("code", data))
      case UnorderedList(items) =>
        tag("ul",
          items.map(i => tag("li", toHtmlString(i))).mkString)
      case OrderedList(items, _) =>
        tag("ol",
          items.map(i => tag("li", toHtmlString(i))).mkString)
      case DefinitionList(items) =>
        tag("dl",
          items.map {
            case (key, value) =>
              tag("dt", toHtmlString(key)) +
                tag("dd", toHtmlString(value))
          }.mkString)
      case HorizontalRule() =>
        tag("hr")
    }

    def toHtmlString(text: Inline): String = text match {
      case Chain(items)        => items.map(toHtmlString).mkString
      case Italic(text)        => tag("emph", toHtmlString(text))
      case Bold(text)          => tag("strong", toHtmlString(text))
      case Underline(text)     => tag("u", toHtmlString(text))
      case Superscript(text)   => tag("sup", toHtmlString(text))
      case Subscript(text)     => tag("sub", toHtmlString(text))
      case Monospace(text)     => tag("code", toHtmlString(text))
      case Link(target, title) => tag("a", Map("href" -> target), toHtmlString(title))
      case Text(text)          => text
      case el: EntityLink      => tag("a", toHtmlString(el.title))
      case HtmlTag(data)       => data
      case Summary(content)    => toHtmlString(content)
    }

    def tag(name: String) =
      s"<$name />"

    def tag(name: String, content: String): String =
      s"<$name>$content</$name>"

    def tag(name: String, attrs: Map[String, String], content: String) = {
      val attrStr = attrs.map { case (k, v) => s"""$k="$v"""" }.mkString(" ")
      s"<$name $attrStr>$content<$name>"
    }
  }
}

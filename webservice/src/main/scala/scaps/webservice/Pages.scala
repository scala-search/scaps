package scaps.webservice

import scalatags.Text.all._
import scalatags.Text.tags2.title
import scalatags.Text.tags2.nav
import scaps.webapi.IndexStatus

object Pages extends Helpers with Bootstrap {
  val pageTitle = "Scaps: Scala API Search"

  def index(status: IndexStatus) =
    html(lang := "en")(
      head(
        meta(charset := "utf-8"),
        meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        title(pageTitle),
        stylesheet("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"),
        stylesheet("css/scaps.css")),
      body(
        nav(cls := "navbar navbar-inverse navbar-fixed-top")(
          container(
            form(cls := "navbar-form navbar-left", role := "search")(
              div(cls := "input-group")(
                span(cls := "input-group-addon", style := "width: 1%;")(span(cls := "glyphicon glyphicon-search")),
                input(tpe := "text", cls := "form-control", placeholder := "Search for Functions, Methods and Values..."))))),
        container(
          h1(pageTitle),
          if (status.workQueue.isEmpty) Seq(
            p("""Scaps is a Scala API search engine for discovering functionality in Scala libraries. You can use both
              type signatures and keywords in your search queries."""),
            p("""Some examples you might want to try:"""),
            ul(
              li(example("max: Int", "An integer value with `max` in it's name or documentation")),
              li(example("max: (Int, Int) => Int", "A function taking two ints and returning Int")),
              li(example("max: Int => Int => Int", "Same query as above but in curried form")),
              li(example("Ordering[String]", "Implementations of the `Ordering` typeclass for strings")),
              li(example("List[A] => (A => Boolean) => List[A]", "A generic query which uses a type parameter `A`")),
              li(example("List => (_ => Boolean) => List", "The identical query as above but with omitted type params"))))
          else
            div(s"building index with ${status.workQueue.size} documents remaining:",
              ul(for { sourceFile <- status.workQueue } yield li(sourceFile))))))

  def example(query: String, desc: String) =
    span(a(code(query)), " - ", desc)
}

trait Helpers {
  def stylesheet(url: String) =
    link(rel := "stylesheet", href := url)
}

trait Bootstrap {
  def container(content: Modifier*) =
    div(cls := "container")(content)
}

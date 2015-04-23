
package scaps.webservice.ui

import scalatags.generic.Bundle
import scalatags.generic.TypedTag
import scaps.webapi.IndexStatus

object HtmlPages extends Pages(scalatags.Text)

class Pages[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) extends Helpers[Builder, Output, FragT] {
  import bundle._
  import bundle.all._
  import bundle.tags2.title
  import bundle.tags2.nav

  val pageTitle = "Scaps: Scala API Search"

  val searchFieldId = "searchField"
  val resultContainerId = "results"
  val boot = s"scaps.webservice.ui.Main().main(document.getElementById('$searchFieldId'), document.getElementById('$resultContainerId'))"

  def index(status: IndexStatus): TypedTag[Builder, Output, FragT] =
    html(lang := "en")(
      head(
        meta(charset := "utf-8"),
        meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        title(pageTitle),
        javascript("api-search-webservice-fastopt.js"),
        stylesheet("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"),
        stylesheet("css/scaps.css")),
      body(onload := boot)(
        nav(cls := "navbar navbar-inverse navbar-fixed-top")(
          div(cls := "container")(
            form(cls := "navbar-form navbar-left", role := "search")(
              div(cls := "input-group")(
                span(cls := "input-group-addon", style := "width: 1%;")(span(cls := "glyphicon glyphicon-search")),
                input(tpe := "text", id := searchFieldId, autofocus, cls := "form-control", placeholder := "Search for Functions, Methods and Values..."))))),
        div(cls := "container", id := resultContainerId)(
          infoPage(status))))

  def infoPage(status: IndexStatus): TypedTag[Builder, Output, FragT] =
    span(
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
          ul(for { sourceFile <- status.workQueue } yield li(sourceFile))))

  def example(query: String, desc: String) =
    span(a(code(query)), " - ", desc)
}

trait Helpers[Builder, Output <: FragT, FragT] {
  val bundle: Bundle[Builder, Output, FragT]
  import bundle._
  import bundle.all._

  def stylesheet(path: String) =
    link(rel := "stylesheet", href := path)

  def javascript(path: String) =
    script(src := path)
}

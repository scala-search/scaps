
package scaps.webservice.ui

import scalatags.generic.Bundle
import scalatags.generic.TypedTag
import scaps.webapi.IndexStatus
import scaps.webapi.TermEntity
import scaps.webapi.TypeEntity
import scaps.webapi.TypeEntity.MemberAccess
import scaps.webapi.TypeParameterEntity

abstract class Pages[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) extends Helpers[Builder, Output, FragT] {
  import bundle._
  import bundle.all._
  import bundle.tags2.title
  import bundle.tags2.nav

  def encodeUri(path: String, params: Map[String, String]): String

  val searchFieldId = "searchField"
  val resultContainerId = "results"
  val main = "scaps.webservice.ui.Main()"
  val boot = s"$main.main(document.getElementById('$searchFieldId'), document.getElementById('$resultContainerId'))"
  val pageTitle = "Scaps: Scala API Search"

  def skeleton(mods: Modifier, query: String = "") =
    html(lang := "en")(
      head(
        meta(charset := "utf-8"),
        meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        title(pageTitle),
        stylesheet("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"),
        stylesheet("css/scaps.css"),
        javascript("api-search-webservice-fastopt.js")),
      body(onload := boot)(
        nav(cls := "navbar navbar-inverse navbar-fixed-top")(
          div(cls := "container")(
            form(cls := "navbar-form navbar-left", method := "get", role := "search")(
              div(cls := "input-group")(
                span(cls := "input-group-addon", style := "width: 1%;")(span(cls := "glyphicon glyphicon-search")),
                input(tpe := "search", name := "q", id := searchFieldId, value := query,
                  autofocus, cls := "form-control", placeholder := "Search for Functions, Methods and Values..."))))),
        div(cls := "container")(
          div(cls := "row")(
            div(cls := "col-md-10 col-md-offset-1", id := resultContainerId)(mods)))))

  def main(status: IndexStatus) = {
    def example(query: String, desc: String) = {
      val uri = encodeUri("", Map("q" -> query))
      li(a(href := uri)(code(query)), " - ", desc)
    }

    div(
      h1(pageTitle),
      if (status.workQueue.isEmpty) Seq(
        p("""Scaps is a Scala API search engine for discovering functionality in Scala libraries. You can use both
              type signatures and keywords in your search queries."""),
        p("""Some examples you might want to try:"""),
        ul(
          example("max: Int", "An integer value with `max` in it's name or documentation"),
          example("max: (Int, Int) => Int", "A function taking two ints and returning Int"),
          example("max: Int => Int => Int", "Same query as above but in curried form"),
          example("Ordering[String]", "Implementations of the `Ordering` typeclass for strings"),
          example("List[A] => (A => Boolean) => List[A]", "A generic query which uses a type parameter `A`"),
          example("List => (_ => Boolean) => List", "The identical query as above but with omitted type params")))
      else
        div(cls := "alert alert-info")(s"building index with ${status.workQueue.size} documents remaining:",
          ul(for { sourceFile <- status.workQueue } yield li(sourceFile))))
  }

  def queryError(msg: String) =
    div(cls := "alert alert-warning")(msg)

  def error(msg: String) =
    div(cls := "alert alert-danger")(msg)

  def results(results: Seq[TermEntity]) =
    dl(results.map(result(_)))

  def result(term: TermEntity) = {
    def typeName(t: TypeEntity) =
      if (term.typeParameters.exists(_.name == t.name))
        em(cls := "type-parameter")(t.name)
      else
        em(a(attrs.title := t.name)(t.shortName))

    def tpe(t: TypeEntity): Modifier = t match {
      case TypeEntity.Function(params, res, _) =>
        val paramTypes =
          if (params.length <= 1) span(params.map(tpe(_)))
          else span("(", intersperse[Modifier](params.map(tpe(_)), ", "), ")")
        span(paramTypes, " => ", tpe(res))
      case TypeEntity.Tuple(tpes, _) =>
        span("(", intersperse[Modifier](tpes.map(tpe(_)), ", "), ")")
      case TypeEntity.Refinement(tpes, _) =>
        span(intersperse[Modifier](tpes.map(tpe(_)), " with "))
      case t @ TypeEntity(_, _, args) =>
        val typeArgs =
          if (args.isEmpty) span()
          else span("[", intersperse[Modifier](args.map(tpe(_)), ", "), "]")
        span(typeName(t), typeArgs)
    }

    def signature(t: TypeEntity): Modifier = t match {
      case TypeEntity.MethodInvocation(args, res, _) =>
        span("(", intersperse[Modifier](args.map(tpe(_)), ", "), ")", signature(res))
      case t =>
        span(": ", tpe(t))
    }

    def typeParams(ps: List[TypeParameterEntity]) =
      if (ps.isEmpty) span()
      else span("[", intersperse[Modifier](ps.map(p => em(cls := "type-parameter")(p.toString)), ", "), "]")

    Seq(
      dt(code(term.tpe match {
        case TypeEntity.MemberAccess(owner, member) =>
          val memberTypeParams = term.typeParameters
            .filterNot(p => owner.toList.exists(_.name == p.name))

          span(tpe(owner), ".", strong(term.shortName), typeParams(memberTypeParams), signature(member))
        case t =>
          span(strong(term.name), typeParams(term.typeParameters), signature(t))
      })),
      dd(div(term.comment),
        div(cls := "label label-default")("scala-library"), " ", term.name))
  }
}

trait Helpers[Builder, Output <: FragT, FragT] {
  val bundle: Bundle[Builder, Output, FragT]
  import bundle._
  import bundle.all._

  def stylesheet(path: String) =
    link(rel := "stylesheet", href := path)

  def javascript(path: String) =
    script(src := path)

  def intersperse[T](ts: Seq[T], t: T): Seq[T] =
    ts.flatMap(e => Seq(e, t)).dropRight(1)
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.webservice.ui

import scalatags.generic.Bundle
import scalatags.generic.TypedTag
import scalatags.stylesheet.Sheet
import scaps.api.IndexStatus
import scaps.api.IndexBusy
import scaps.api.ValueDef
import scaps.api.TypeRef
import scaps.api.TypeRef.MemberAccess
import scaps.api.TypeParameter
import scaps.api.ScapsApi
import scaps.buildInfo.BuildInfo
import scaps.api.Result

abstract class Pages[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT])
    extends Helpers[Builder, Output, FragT] {

  import bundle._
  import bundle.all._
  import bundle.tags2.title
  import bundle.tags2.nav

  def encodeUri(path: String, params: List[(String, String)]): String

  def prodMode: Boolean = false

  def analyticsScript: Option[String] = None

  val pageTitle = "Scaps: Scala API Search"

  val aboutUrl = "http://about.scala-search.org/"

  object jsCallbacks {
    val main = "scaps.webservice.ui.Main()"

    def boot(searchFormId: String, resultContainerId: String) =
      s"$main.main(document.getElementById('$searchFormId'), document.getElementById('$resultContainerId'))"

    def assessPositively(feedbackElementId: String, value: ValueDef) =
      s"$main.assessPositively(document.getElementById('$feedbackElementId'), '${value.signature}')"
  }

  def searchUri(query: String, enabledModuleIds: Set[String] = Set(), page: Int = 0) = {
    val params = List[(String, String)]() ++
      (if (query == "") None else Some("q" -> query)) ++
      (if (page == 0) None else Some("p" -> page.toString)) ++
      enabledModuleIds.map("m" -> _)
    encodeUri("", params)
  }

  def skeleton(status: IndexStatus, enabledModuleIds: Set[String], mods: Modifier,
               query: String = "") = {
    val searchFormId = "searchField"
    val resultContainerId = "results"

    html(lang := "en")(
      head(
        meta(charset := "utf-8"),
        meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        title(pageTitle),
        stylesheet("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"),
        stylesheet("docStyles.css"),
        javascript("https://code.jquery.com/jquery-2.1.4.min.js"),
        javascript("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"),
        if (prodMode)
          javascript("scaps-webservice-ui-opt.js")
        else
          javascript("scaps-webservice-ui-fastopt.js")),

      body(paddingTop := 90.px, paddingBottom := 41.px, onload := jsCallbacks.boot(searchFormId, resultContainerId))(
        form(id := searchFormId, method := "get", role := "search")(
          nav(cls := "navbar navbar-inverse navbar-fixed-top")(
            div(cls := "container")(
              div(cls := "navbar-header", width := 100.pct)(
                a(cls := "navbar-brand", href := "/")("Scaps"),
                div(cls := "form-group", display.inline)(
                  div(cls := "input-group", display.table, marginTop := 8.px, marginRight := 8.px)(
                    span(cls := "input-group-addon", width := 1.pct)(span(cls := "glyphicon glyphicon-search")),
                    input(tpe := "search", name := "q", value := query, autocomplete := "off",
                      autofocus, cls := "form-control", placeholder := "Search for Functions, Methods and Values...")))))),
          nav(marginTop := 50.px, minHeight := 0.px, cls := s"navbar navbar-default navbar-fixed-top")(
            div(cls := "container")(
              ul(marginTop := 3.px, marginBottom := 5.px) {
                val disabledAttr = when(status.indexedModules.length <= 1) { disabled }

                status.indexedModules.sortBy(_.name).map { m =>
                  li(display.inline, paddingRight := 20.px) {
                    val checkedAttr = when(enabledModuleIds.contains(m.moduleId)) { checked := true }

                    label(cls := "checkbox-inline")(
                      input(tpe := "checkbox", name := "m", value := m.moduleId, checkedAttr, disabledAttr),
                      s"${m.name}:${m.revision} ")
                  }
                }
              }))),

        div(cls := "container")(
          div(cls := "row")(
            div(cls := "col-md-10 col-md-offset-1", id := resultContainerId)(mods))),

        nav(cls := "navbar navbar-default navbar-fixed-bottom", minHeight := 0.px)(
          div(cls := "navbar-text", width := 100.pct, textAlign.center, marginTop := 4.px, marginBottom := 4.px) {
            val statusInfo = status match {
              case IndexBusy(_, _) => s", Index is Updating"
              case _               => ""
            }
            span(
              s"by Lukas Wegmann | version ${BuildInfo.version}$statusInfo | ",
              a(href := aboutUrl)("About"))
          }),

        raw(analyticsScript.getOrElse(""))))
  }

  def main(status: IndexStatus, enabledModuleIds: Set[String]) = {
    def example(query: String, desc: String) = {
      p(a(href := searchUri(query, enabledModuleIds))(code(query)), " - ", desc)
    }

    div(
      h1(pageTitle),
      p("""Scaps is a search engine for discovering functionality in Scala libraries (or in other words, a """,
        a(href := "https://www.haskell.org/hoogle/")("Hoogle"), """ for Scala). You can use both
        type signatures and keywords in your search queries."""),
      div(cls := "panel panel-default")(
        div(cls := "panel-heading")(
          h2(cls := "panel-title")("Example Queries")),
        div(cls := "panel-body")(
          example("max: Int", "An integer value with `max` in it's name or doc comment."),
          example("max: (Int, Int) => Int", "A function taking two ints and returning Int."),
          example("max: Int => Int => Int", "Same query as above but in curried form."),
          example("Ordering[String]", "Implementations of the `Ordering` typeclass for strings."),
          example("List[A] => Int => Option[A]", "A generic query which uses a type parameter `A`. All type identifiers consisting of a single character are treated as type parameters."),
          example("List => Int => Option", "The identical query as above but with omitted type parameters."),
          example("&~", "Searches for symbolic operators are also possible."))),
      div(cls := "panel panel-default")(
        div(cls := "panel-heading")(
          h2(cls := "panel-title")(
            a(href := aboutUrl)(span(cls := "glyphicon glyphicon-link"), " About Scaps"))),
        div(cls := "panel-body")(
          "Scaps is an offspring of a master's thesis by Lukas Wegmann at the ",
          a(href := "http://www.hsr.ch/")("University of Applied Science Rapperswil (HSR)."))))
  }

  def queryError(msg: String) =
    div(cls := "alert alert-warning")(msg)

  def error(msg: String) =
    div(cls := "alert alert-danger")(msg)

  def groupedResults(query: String, enabledModuleIds: Set[String], results: Seq[(ValueDef, Seq[Result[ValueDef]])]): Modifier =
    div(results.toList.map((group _).tupled))

  def group(key: ValueDef, results: Seq[Result[ValueDef]]): Modifier = {
    def prty(tpe: TypeRef): String = tpe match {
      case TypeRef.Function(a :: Nil, res, _) => prty(a) + " => " + prty(res)
      case TypeRef.Implicit(a, _)             => "?" + prty(a)
      case TypeRef.Repeated(a, _)             => prty(a) + "*"
      case t                                  => t.name
    }

    val keyId = key.signature(true).hashCode().toString

    val bestMatch = dl(marginBottom := 10.px, result(results.head))

    val remaining =
      if (results.tail.isEmpty)
        None
      else {
        Some(
          div(cls := "panel panel-default",
            div(cls := "panel-heading",
              a(data("toggle") := "collapse", href := "#" + keyId, aria.expanded := false, aria.controls := keyId,
                results.tail.length + " more results matching ", strong(key.name + ": " + prty(key.tpe)))),
            div(id := keyId, cls := "panel-collapse collapse",
              div(cls := "panel-body",
                dl(results.tail.map(result))))))
      }

    div(bestMatch, remaining)
  }

  def result(res: Result[ValueDef]) = {
    val typeParamStyle = color := "#999999"
    val value = res.entity

    def typeName(t: TypeRef) =
      if (value.typeParameters.exists(_.name == t.name))
        em(typeParamStyle)(t.name)
      else
        em(a(attrs.title := t.name)(t.shortName))

    def tpe(t: TypeRef): Modifier = t match {
      case TypeRef.Function(params, res, _) =>
        val paramTypes =
          span("(", intersperse[Modifier](params.map(tpe(_)), ", "), ")")
        span(paramTypes, " => ", tpe(res))
      case TypeRef.Tuple(tpes, _) =>
        span("(", intersperse[Modifier](tpes.map(tpe(_)), ", "), ")")
      case TypeRef.Refinement(tpes, _) =>
        span(intersperse[Modifier](tpes.map(tpe(_)), " with "))
      case TypeRef.ByName(arg, _) =>
        span(" => ", tpe(arg))
      case TypeRef.Repeated(arg, _) =>
        span(tpe(arg), "*")
      case TypeRef.Implicit(arg, _) =>
        span("implicit ", tpe(arg))
      case t @ TypeRef(_, _, args, _) =>
        val typeArgs =
          if (args.isEmpty) span()
          else span("[", intersperse[Modifier](args.map(tpe(_)), ", "), "]")
        span(typeName(t), typeArgs)
    }

    def signature(t: TypeRef): Modifier = t match {
      case TypeRef.MethodInvocation(args, res, _) =>
        span("(", intersperse[Modifier](args.map(tpe(_)), ", "), ")", signature(res))
      case t =>
        span(": ", tpe(t))
    }

    def typeParams(ps: List[TypeParameter]) =
      if (ps.isEmpty) span()
      else span("[", intersperse[Modifier](ps.map(p => em(typeParamStyle)(p.toString)), ", "), "]")

    val docLink = {
      value.docLink.map { lnk =>
        a(href := lnk)("Doc")
      }
    }

    val feedback = {
      val feedbackElemId = s"feedback_${value.signature}"

      div(id := feedbackElemId, display.inline)(
        a(href := "javascript:void(0);",
          onclick := jsCallbacks.assessPositively(feedbackElemId, value))(
            span(cls := "glyphicon glyphicon-thumbs-up"), " This is what i've been looking for"))
    }

    val info = intersperse[Modifier](docLink.toSeq ++ Seq(feedback), raw(" &middot; "))

    Seq(
      dt(paddingTop := 20.px)(
        code(value.tpe match {
          case TypeRef.MemberAccess(owner, member) =>
            val memberTypeParams = value.typeParameters
              .filterNot(p => owner.toList.exists(_.name == p.name))

            span(tpe(owner), ".", strong(value.shortName), typeParams(memberTypeParams), signature(member))
          case t =>
            span(strong(value.name), typeParams(value.typeParameters), signature(t))
        }), small(float.right)(res.score)),
      dd(
        res.explanation.map(e => pre(e)),
        div(cls := "docComment")(
          raw(value.comment.body),
          dl(value.comment.attributes.toList.flatMap {
            case (key, value) =>
              List(dt(raw(key)), dd(raw(value)))
          })),
        div(span(cls := "label label-default")(value.module.name), " ", value.name),
        info))
  }

  val feedbackFeedbackStyle = color := "#999999"

  def feedbackReceived =
    span(feedbackFeedbackStyle)(raw("Thank you for your feedback &#9786;"))

  def feedbackError =
    span(feedbackFeedbackStyle)("Sorry, there was an error transferring your feedback.")
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

  def when(cond: Boolean)(content: => Modifier): Modifier =
    if (cond) content
    else ""
}

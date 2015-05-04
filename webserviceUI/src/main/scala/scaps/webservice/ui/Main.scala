package scaps.webservice.ui

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.ext._
import org.scalajs.dom.html
import org.scalajs.dom.raw.FocusEvent
import autowire._
import scaps.webapi.ScapsApi
import org.scalajs.dom.raw.Event

@JSExport
object Main {
  val scaps = new AjaxClient(ScapsApi.apiPath)[ScapsApi]

  val globalState = PageState.empty

  @JSExport
  def main(searchForm: html.Form, container: html.Div) = {
    val searchField = searchForm.querySelector("[name=q]").asInstanceOf[html.Input]
    val moduleCheckboxes = searchForm.querySelectorAll("[name=m]").map(_.asInstanceOf[html.Input])

    searchField.addEventListener("focus", (_: FocusEvent) => {
      searchField.select()
    })

    { // init query state
      def getQuery() =
        searchField.value.trim

      globalState.query() = getQuery()
      val q = Variable(getQuery())

      searchField.addEventListener("keyup", {
        (_: dom.KeyboardEvent) =>
          q() = getQuery()
      })

      Observable.debounce(400.millis)(q).foreach { query =>
        globalState.query() = query
      }
    }

    { // init modules state
      def getModule() =
        moduleCheckboxes
          .filter(checkBox => checkBox.checked && !checkBox.value.isEmpty())
          .map(_.value).headOption

      globalState.moduleId() = getModule()

      moduleCheckboxes.foreach { checkbox =>
        checkbox.addEventListener("change", (_: Event) => {
          globalState.moduleId() = getModule()
        })
      }
    }

    mainContent.foreach(content => replaceContent(container, content.render))
  }

  val queryWithModules = Observable.join(globalState.query, globalState.moduleId)

  val mainContent = Observable.async(queryWithModules.map((fetchContent _).tupled))

  @JSExport
  def assessPositively(feedbackElement: html.Div, resultNo: Int, signature: String): Unit = {
    scaps.assessPositivley(globalState.query(), globalState.moduleId(), resultNo, signature).call()
      .map(_ => DomPages.feedbackReceived)
      .recover { case _ => DomPages.feedbackError }
      .foreach(answer => replaceContent(feedbackElement, answer.render))
  }

  def fetchContent(query: String, moduleId: Option[String]) = {
    if (query.isEmpty()) {
      scaps.getStatus().call().map(DomPages.main(_))
    } else {
      scaps.search(query, moduleId).call().map {
        case Left(msg)      => DomPages.queryError(msg)
        case Right(results) => DomPages.results(0, query, moduleId, results)
      }
    }.recover {
      case AjaxException(_) => DomPages.error("The Scaps service is currently unreachable. Please try again later.")
    }
  }

  def replaceContent(outer: dom.Element, content: dom.Element): Unit = {
    outer.innerHTML = ""
    outer.appendChild(content)
    ()
  }
}

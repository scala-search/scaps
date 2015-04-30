package scaps.webservice.ui

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.html
import org.scalajs.dom.raw.FocusEvent

import autowire._
import scaps.webapi.ScapsApi

@JSExport
object Main {
  val scaps = new AjaxClient(ScapsApi.apiPath)[ScapsApi]

  @JSExport
  def main(searchField: html.Input, container: html.Div) = {
    searchField.addEventListener("focus", (_: FocusEvent) => {
      searchField.select()
    })

    val query = {
      val q = Variable(searchField.value)

      searchField.addEventListener("keyup", {
        (_: dom.KeyboardEvent) =>
          q() = searchField.value.trim()
      })

      Observable.debounce(400.millis)(q)
    }

    Observable.async(query.map(fetchContent))
      .foreach(content => replaceContent(container, content.render))
  }

  @JSExport
  def assessPositively(feedbackElement: html.Div, query: String, signature: String): Unit = {
    scaps.assessPositivley(query, signature).call()
      .map(_ => DomPages.feedbackReceived)
      .recover { case _ => DomPages.feedbackError }
      .foreach(answer => replaceContent(feedbackElement, answer.render))
  }

  def fetchContent(query: String) = {
    if (query.isEmpty()) {
      scaps.getStatus().call().map(DomPages.main(_))
    } else {
      scaps.search(query).call().map {
        case Left(msg)      => DomPages.queryError(msg)
        case Right(results) => DomPages.results(0, query, results)
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

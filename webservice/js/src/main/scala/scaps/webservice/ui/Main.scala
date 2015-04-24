package scaps.webservice.ui

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.Any.fromFunction0
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html
import autowire._
import scalatags.JsDom.all._
import scaps.webapi.IndexStatus
import scaps.webapi.ScapsApi
import scaps.webapi.SearchResult
import scaps.webapi.SearchResult
import scalatags.JsDom
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.raw.PopStateEvent
import org.scalajs.dom.raw.FocusEvent

@JSExport
object Main {
  val scaps = Ajaxer[ScapsApi]

  @JSExport
  def main(searchField: html.Input, container: html.Div) = {
    searchField.addEventListener("focus", (_: FocusEvent) => {
      searchField.select()
    })

    searchField.addEventListener("keyup", debounce(400) {
      event: dom.KeyboardEvent =>
        handleQuery(searchField.value, container)
    })
  }

  def debounce[A](ms: Int)(f: A => Unit): A => Unit = {
    var handle: Int = 0
    (a: A) => {
      dom.clearTimeout(handle)
      handle = dom.setTimeout(() => {
        f(a)
      }, ms)
    }
  }

  def handleQuery(query: String, container: html.Div) = {
    val content = if (query.isEmpty()) {
      scaps.getStatus().call().map(DomPages.main(_))
    } else {
      scaps.search(query).call().map {
        case Left(msg)      => DomPages.error(msg)
        case Right(results) => DomPages.results(results)
      }
    }.recover {
      case AjaxException(_) => DomPages.connectionError()
    }

    content.foreach(replaceContent(container, _))
  }

  def replaceContent(container: html.Div, content: scalatags.generic.TypedTag[dom.Element, dom.Element, dom.Node]) = {
    container.innerHTML = ""
    container.appendChild(content.render)
  }
}

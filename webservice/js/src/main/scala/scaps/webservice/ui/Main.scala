package scaps.webservice.ui

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.Any.fromFunction0
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom
import org.scalajs.dom.html

import autowire._
import scalatags.JsDom.all._
import scaps.webapi.IndexStatus
import scaps.webapi.ScapsApi
import scaps.webapi.SearchResult

object DomPages extends Pages(scalatags.JsDom)

@JSExport
object Main {
  val scaps = Ajaxer[ScapsApi]

  @JSExport
  def main(searchField: html.Input, container: html.Div) {
    searchField.onkeyup = debounced(200) {
      event: dom.KeyboardEvent =>
        handle(searchField.value, container)
    }
  }

  def debounced[A](ms: Int)(f: A => Unit): A => Unit = {
    var handle: Int = 0
    (a: A) => {
      dom.clearTimeout(handle)
      handle = dom.setTimeout(() => {
        f(a)
      }, ms)
    }
  }

  def handle(query: String, container: html.Div) = {
    if (query.isEmpty()) {
      scaps.getStatus().call().foreach(formatStatus(container, _))
    } else {
      scaps.search(query).call().foreach {
        case Left(msg)      => formatError(container, msg)
        case Right(results) => formatResults(container, results)
      }
    }
  }

  def formatStatus(container: html.Div, status: IndexStatus) = {
    container.innerHTML = ""
    container.appendChild(DomPages.infoPage(status).render)
  }

  def formatError(container: html.Div, msg: String) = {
    container.innerHTML = ""
    container.appendChild(span(msg).render)
  }

  def formatResults(container: html.Div, results: Seq[SearchResult]) = {
    val content = ul(
      results.map { result => li(result.signature) })
    container.innerHTML = ""
    container.appendChild(content.render)
  }
}

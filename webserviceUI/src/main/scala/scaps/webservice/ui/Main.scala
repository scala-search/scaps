package scaps.webservice.ui

import scala.concurrent.duration.DurationInt
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.ext._
import org.scalajs.dom.html
import org.scalajs.dom.raw.FocusEvent
import autowire._
import scaps.webapi.ScapsApi
import org.scalajs.dom.raw.Event
import monifu.reactive._
import monifu.reactive.subjects.PublishSubject
import monifu.reactive.Observable.FutureIsObservable
import monifu.concurrent.Implicits.globalScheduler
import monifu.concurrent.Scheduler
import scaps.webservice.ui.ObservableExtensions._

@JSExport
object Main {
  val scaps = new AjaxClient(ScapsApi.apiPath)[ScapsApi]

  val searchForm = PublishSubject[html.Form]()
  val mainContainer = PublishSubject[html.Div]()

  @JSExport
  def main(form: html.Form, container: html.Div) = {
    implicitly[Scheduler].execute {
      searchForm.onNext(form)
      mainContainer.onNext(container)
      ()
    }
  }

  val searchField = searchForm.map(_.querySelector("[name=q]").asInstanceOf[html.Input])

  val moduleCheckboxes = searchForm.map(form =>
    form.querySelectorAll("[name=m]").map(_.asInstanceOf[html.Input]))

  searchField
    .foreach { sf =>
      sf.focus()
      sf.selectionStart = sf.value.length()
      sf.selectionEnd = sf.value.length()
    }

  val query = searchField
    .map(_.value)
    .map(_.trim)

  val moduleIds = moduleCheckboxes
    .flatMap(_.observeValues())
    .distinctUntilChanged

  Observable.combineLatest(query, moduleIds)
    .drop(1)
    .foreach {
      case (q, ms) =>
        dom.location.href = DomPages.searchUri(q, ms)
    }

  val positiveAssessment = PublishSubject[(html.Div, Int, String)]

  @JSExport
  def assessPositively(feedbackElement: html.Div, resultNo: Int, signature: String): Unit = {
    positiveAssessment.onNext((feedbackElement, resultNo, signature))
    ()
  }

  Observable.combineLatest(query, moduleIds, positiveAssessment).foreach {
    case (query, moduleIds, (feedbackElem, resultNo, signature)) =>
      scaps.assessPositivley(query, moduleIds, resultNo, signature).call()
        .map(_ => DomPages.feedbackReceived)
        .recover { case _ => DomPages.feedbackError }
        .foreach(answer => replaceContent(feedbackElem, answer.render))
  }

  def replaceContent(outer: dom.Element, content: dom.Element): Unit = {
    outer.innerHTML = ""
    outer.appendChild(content)
    ()
  }
}

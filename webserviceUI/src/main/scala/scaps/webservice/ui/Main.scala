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

  val searchFieldChanges = Observable.merge[Any](
    searchField,
    searchField.mergeMap(_ observeDomEvents ("keyup")))

  val moduleChanges = Observable.merge[Any](
    moduleCheckboxes,
    moduleCheckboxes.flatMap { checkboxes =>
      Observable.merge(checkboxes.map(_.observeDomEvents("change")): _*)
    })

  for {
    field <- searchField
    _ <- field.observeDomEvents("focus")
  } {
    field.select()
  }

  val query =
    Observable.combineLatest(searchField, searchFieldChanges)
      .map {
        case (field, _) => field.value.trim
      }
      .distinctUntilChanged
      .debounce(400.millis)
      .dump("query")

  val moduleIds =
    Observable.combineLatest(moduleCheckboxes, moduleChanges)
      .map {
        case (checkboxes, _) => checkboxes.filter(_.checked).map(_.value).toSet
      }
      .dump("moduleIds")

  val staticContent = mainContainer.map(_.childNodes.head)

  val dynamicContent =
    Observable.combineLatest(query, moduleIds)
      .drop(1)
      .flatMap {
        case (q, ms) => fetchContent(q, ms)
      }
      .map(_.render)

  Observable.combineLatest(dynamicContent, mainContainer).foreach {
    case (content, container) => replaceContent(container, content)
  }

  val content = Observable.merge(staticContent, dynamicContent)

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

  def fetchContent(query: String, moduleIds: Set[String]) = {
    if (query.isEmpty()) {
      scaps.getStatus().call().map(DomPages.main(_, moduleIds))
    } else {
      scaps.search(query, moduleIds).call().map {
        case Left(msg)      => DomPages.queryError(msg)
        case Right(results) => DomPages.results(0, query, moduleIds, results)
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

package scaps.webservice.ui

import scala.concurrent.duration.DurationInt
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
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

  val searchForm = Variable[html.Form]()
  val mainContainer = Variable[html.Div]()

  @JSExport
  def main(form: html.Form, container: html.Div) = {
    searchForm() = form
    mainContainer() = container
  }

  val searchField = searchForm.map(_.querySelector("[name=q]").asInstanceOf[html.Input])

  for {
    field <- searchField
    _ <- Observable.fromDomEvents(field, "focus")
  } {
    field.select()
  }

  val query = {
    val fieldOrFieldValueChanges =
      Observable.merge[Any](searchField,
        Observable.debounce(400.millis)(
          Observable.fromDomEvents(searchField, "keyup")))

    Observable.join(searchField, fieldOrFieldValueChanges).map {
      case (field, _) => field.value.trim
    }
  }

  val moduleIds = searchForm.flatMap { form =>
    val moduleCheckboxes = form.querySelectorAll("[name=m]").map(_.asInstanceOf[html.Input])

    def getModules() =
      moduleCheckboxes
        .filter(checkBox => checkBox.checked)
        .map(_.value).toSet

    val ms = Variable[Set[String]]()
    ms() = getModules()

    moduleCheckboxes.foreach { checkbox =>
      checkbox.addEventListener("change", (_: Event) => {
        ms() = getModules()
      })
    }

    ms
  }

  val mainContent = Observable.async(
    Observable.join(query, moduleIds).map((fetchContent _).tupled))
    .map(_.get)

  Observable.join(mainContent, mainContainer).foreach {
    case (content, container) => replaceContent(container, content.render)
  }

  val positiveAssessment = Variable[(html.Div, Int, String)]

  @JSExport
  def assessPositively(feedbackElement: html.Div, resultNo: Int, signature: String): Unit = {
    positiveAssessment() = (feedbackElement, resultNo, signature)
  }

  Observable.join(query, moduleIds, positiveAssessment).foreach {
    case (query, moduleIds, (feedbackElem, resultNo, signature)) =>
      println(query)
      println(moduleIds)
      println(feedbackElem)
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

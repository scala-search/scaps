package scaps.webservice.ui

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html
import monifu.reactive._
import scala.concurrent.duration._
import monifu.concurrent.Scheduler
import monifu.concurrent.Cancelable

object ObservableExtensions {
  implicit class ObservableOps[T](observable: Observable[T]) {
    def debounce(delay: FiniteDuration): Observable[T] =
      Observable.create { subscriber =>
        val debounceObserver =
          new DebounceObserver(subscriber.observer, delay, subscriber.scheduler)

        observable.unsafeSubscribe(debounceObserver)(subscriber.scheduler)
        ()
      }
  }

  private class DebounceObserver[T](downstream: Observer[T], delay: FiniteDuration, scheduler: Scheduler) extends Observer[T] {
    @volatile
    private[this] var isDone = false

    @volatile
    private[this] var cancelable = Cancelable()

    def onNext(elem: T): Ack = {
      if (!isDone) {
        cancelable.cancel()

        cancelable = scheduler.scheduleOnce(delay) {
          downstream.onNext(elem)
          ()
        }

        Ack.Continue
      } else {
        Ack.Cancel
      }
    }

    def onComplete(): Unit = {
      isDone = true

      scheduler.scheduleOnce(delay) {
        downstream.onComplete()
      }

      ()
    }

    def onError(ex: Throwable): Unit = {
      isDone = true

      cancelable.cancel()

      downstream.onError(ex)
    }
  }

  implicit class ObservableDomNodes[N <: dom.Node](node: N) {
    def observeDomEvents(eventName: String): Observable[N] =
      Observable.create { subscriber =>
        implicit val executionContext = subscriber.scheduler

        def loop(): Unit = {
          lazy val listener: js.Function1[dom.Event, Unit] = (e: dom.Event) => {
            subscriber.scheduler.execute {
              subscriber.observer.onNext(node).onSuccess {
                case Ack.Continue =>
                  node.removeEventListener(eventName, listener)
                  loop()
                case Ack.Cancel =>
                  node.removeEventListener(eventName, listener)
              }
              ()
            }
          }

          node.addEventListener(eventName, listener)
        }

        loop()
      }
  }

  implicit class ObservableInputElements(input: html.Input) {
    def observeValue(): Observable[String] = {
      val changeEvent = input.`type` match {
        case "text" | "search" => input.observeDomEvents("input")
        case "checkbox"        => input.observeDomEvents("change")
        case t                 => throw new NotImplementedError(s"observe changes on $t")
      }

      Observable.concat(
        Observable.unit(input.value),
        changeEvent.map(_ => input.value))
    }
  }

  implicit class ObservableCheckboxGroup(checkboxes: Seq[html.Input]) {
    def observeValues(): Observable[Set[String]] = {
      val changeEvents = Observable.fromIterable(checkboxes.map(_.observeValue())).merge()

      def getValues = checkboxes.filter(_.checked).map(_.value).toSet

      Observable.concat(
        Observable.unit(getValues),
        changeEvents.map(_ => getValues))
    }
  }
}

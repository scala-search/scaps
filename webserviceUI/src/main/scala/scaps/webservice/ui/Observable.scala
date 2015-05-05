package scaps.webservice.ui

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import org.scalajs.dom
import scala.scalajs.concurrent.JSExecutionContext
import scala.util.Try
import scala.util.Success
import scala.util.Failure

/**
 * A small utility library to encapsulate change propagations inspired by scala.Rx
 * and similar reactive libraries.
 *
 * Note, that this implementation is not suited for multithreaded environments and
 * is (currently) only designed for constructing static dataflow graphs. Other uses
 * may lead to memory leaks and unexpected behavior.
 */
trait Observable[+T] { outer =>
  import Observable._

  private var observers: List[Any => Unit] = Nil

  protected def notifyObservers(v: Any) = {
    observers.foreach(_(v))
  }

  /** Register an observer that is called whenever a new value is propagated */
  def foreach(handler: T => Unit): Unit = {
    observers = handler.asInstanceOf[Any => Unit] :: observers
    println(s"added handler to $this (${observers.length} handlers)")
  }

  def map[A](f: T => A): Observable[A] = {
    val v = Variable[A]
    foreach(t => v.updateBlocking(f(t)))
    v
  }

  def flatMap[A](f: T => Observable[A]): Observable[A] =
    Observable.flatten(outer.map(f))
}

object Observable {
  // hardwirering the execution context makes some things easier for now
  implicit val executionContext = JSExecutionContext.queue

  /** Creates an observable that fires each time a future in `o` is completed. */
  def async[T](o: Observable[Future[T]]): Observable[Try[T]] =
    o.flatMap(from(_))

  def from[T](f: Future[T]): Observable[Try[T]] = {
    val v = Variable[Try[T]]
    f.onComplete(v.updateBlocking)
    v
  }

  /**
   * Creates a new Observable that will not be updated more than once in the
   *  time period `duration`
   */
  def debounce[T](duration: Duration)(o: Observable[T]): Observable[T] = {
    val v = Variable[T]
    var handle = 0

    o.foreach { res =>
      dom.clearTimeout(handle)
      handle = dom.setTimeout(() => {
        v.updateBlocking(res)
      }, duration.toMillis)
    }

    v
  }

  def flatten[A](os: Observable[Observable[A]]): Observable[A] = {
    val v = Variable[A]()
    os.foreach { o =>
      o.foreach { x =>
        v.updateBlocking(x)
      }
    }
    v
  }

  def join[A, B](oa: Observable[A], ob: Observable[B]): Observable[(A, B)] = {
    var a: Option[A] = None
    var b: Option[B] = None
    val v = Variable[(A, B)]

    def propagate() =
      for {
        va <- a
        vb <- b
      } {
        v.updateBlocking((va, vb))
      }

    oa.foreach { va =>
      a = Some(va)
      propagate()
    }

    ob.foreach { vb =>
      b = Some(vb)
      propagate()
    }

    v
  }

  def joinWith[A, B, C](oa: Observable[A], ob: Observable[B])(f: (A, B) => C): Observable[C] =
    join(oa, ob).map {
      case (a, b) => f(a, b)
    }

  def join[A, B, C](oa: Observable[A], ob: Observable[B], oc: Observable[C]): Observable[(A, B, C)] =
    join(oa, join(ob, oc)).map {
      case (a, (b, c)) => (a, b, c)
    }

  def merge[A](oa: Observable[A], ob: Observable[A]): Observable[A] = {
    val v = Variable[A]()
    oa.foreach { v.updateBlocking }
    ob.foreach { v.updateBlocking }
    v
  }

  def fromDomEvents(elem: dom.Element, eventType: String): Observable[dom.Event] = {
    val v = Variable[dom.Event]

    elem.addEventListener(eventType, (e: dom.Event) => {
      v.updateBlocking(e)
    })

    v
  }

  def fromDomEvents(o: Observable[dom.Element], eventType: String): Observable[dom.Event] =
    o.flatMap(fromDomEvents(_, eventType))

  def single[A](a: A): Observable[A] = {
    val v = Variable[A]
    v() = a
    v
  }
}

/**
 * An observable that can be updated.
 */
class Variable[T] extends Observable[T] { outer =>
  import Observable._

  private var v: Option[T] = None

  println(s"init variable $this")

  def update(newV: T) = {
    println(s"enqueue $this = ${newV.toString.take(100)}")
    executionContext.execute(new Runnable {
      def run() = {
        println(s"execute $outer = ${newV.toString.take(100)}")
        v = Some(newV)
        notifyObservers(newV)
      }
    })
  }

  def updateBlocking(newV: T) = {
    println(s"execute blocking $outer = ${newV.toString.take(100)}")
    v = Some(newV)
    notifyObservers(newV)
  }
}

object Variable {
  //def apply[T](init: T) = new Variable[T](Some(init))

  def apply[T]() = new Variable[T]
}

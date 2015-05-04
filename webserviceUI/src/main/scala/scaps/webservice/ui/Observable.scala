package scaps.webservice.ui

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

import org.scalajs.dom

/**
 * A small utility library to encapsulate change propagations inspired by scala.Rx
 * and similar reactive libraries.
 *
 * Note, that this implementation is not suited for multithreaded environments and
 * is (currently) only designed for constructing static dataflow graphs. Other uses
 * may lead to memory leaks and unexpected behavior.
 */
trait Observable[T] { outer =>
  private var observers: List[T => Unit] = Nil
  private var current: Option[T] = None

  protected def notifyObservers(v: T) = {
    current = Some(v)
    observers.foreach(_(v))
  }

  /** Register an observer that is called whenever a new value is propagated */
  def foreach(handler: T => Unit): Unit = {
    observers = handler :: observers
  }

  def tryGet(): Option[T] = current

  def map[A](f: T => A): Observable[A] =
    new Observable[A] {
      override def tryGet() = outer.tryGet().map(f)

      override def foreach(handler: A => Unit) = {
        outer.foreach(f andThen handler)
      }
    }
}

object Observable {
  /** Creates an observable that fires each time a future in `o` is completed. */
  def async[T](o: Observable[Future[T]])(implicit ec: ExecutionContext): Observable[T] =
    new Observable[T] {
      override def foreach(handler: T => Unit) = {
        o.foreach { f =>
          f.foreach(handler)
        }
      }
    }

  /**
   * Creates a new Observable that will not be updated more than once in the
   *  time period `duration`
   */
  def debounce[T](duration: Duration)(o: Observable[T]): Observable[T] =
    new Observable[T] {
      override def tryGet() = o.tryGet()

      override def foreach(handler: T => Unit) = {
        var handle = 0
        o.foreach { (v: T) =>
          dom.clearTimeout(handle)
          handle = dom.setTimeout(() => handler(v), duration.toMillis)
        }
      }
    }

  def join[A, B](oa: Observable[A], ob: Observable[B]): Observable[(A, B)] =
    new Observable[(A, B)] {
      override def tryGet() =
        for {
          a <- oa.tryGet()
          b <- ob.tryGet()
        } yield (a, b)

      override def foreach(handler: ((A, B)) => Unit) = {
        var a: Option[A] = oa.tryGet()
        var b: Option[B] = ob.tryGet()
        oa.foreach { va =>
          a = Some(va)
          for {
            vb <- b
          } {
            handler((va, vb))
          }
        }
        ob.foreach { vb =>
          b = Some(vb)
          for {
            va <- a
          } {
            handler((va, vb))
          }
        }
      }
    }

  def joinWith[A, B, C](oa: Observable[A], ob: Observable[B])(f: (A, B) => C): Observable[C] =
    join(oa, ob).map {
      case (a, b) => f(a, b)
    }
}

/**
 * An observable that can be updated and always holds a value.
 *
 * A Variable fires the first time when it is set to a value that is not equal
 * to the initial value `init`.
 */
class Variable[T](init: T) extends Observable[T] {
  private var v: T = init

  override def tryGet() = Some(v)

  def update(newV: T) = {
    if (v != newV) {
      v = newV
      notifyObservers(newV)
    }
  }

  def apply() = {
    v
  }
}

object Variable {
  def apply[T](init: T) = new Variable[T](init)
}

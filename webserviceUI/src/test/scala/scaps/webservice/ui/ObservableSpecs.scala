package scaps.webservice.ui

import utest._
import org.scalajs.dom
import scala.concurrent.duration._
import scala.concurrent.Promise
import scala.util.Try

object ObservableSpecs extends TestSuite {
  val tests = TestSuite {
    'variables{
      'update{
        val v = Variable[Int]
        var r = 0
        v.foreach(i => r = i)
        v() = 1

        enqueue {
          assert(r == 1)
        }
      }
      'updateBeforeSubscription{
        val v = Variable[Int]
        var r = 0
        v() = 1

        enqueue { v.foreach(i => r = i) }

        enqueue {
          assert(r != 1)
        }
      }
    }

    'observables{
      val v = Variable[Int]
      var r = 0

      'map{
        val o = v.map(_ + 1)
        o.foreach(i => r = i)
        v() = 1

        enqueue {
          assert(r == 2)
        }
      }

      'flatten{
        val v = Variable[Variable[Int]]

        val o = Observable.flatten(v)
        o.foreach(i => r = i)

        val inner = Variable[Int]
        v() = inner
        inner() = 100

        enqueue {
          enqueue {
            assert(r == 100)
          }
        }
      }

      'flatMap{
        val o = v.flatMap { i =>
          val inner = Variable[Int]
          inner() = i
          inner
        }
        o.foreach(i => r = i)
        v() = 2

        enqueue {
          enqueue {
            assert(r == 2)
          }
        }
      }

      'join{
        val v2 = Variable[String]
        var r = (0, "")

        val o = Observable.join(v, v2)
        o.foreach { x => r = x }

        v() = 1

        enqueue {
          assert(r == ((0, "")))
        }

        v2() = "hi"

        enqueue {
          enqueue {
            assert(r == ((1, "hi")))
          }
        }
      }

      'merge{
        val v2 = Variable[Int]

        val o = Observable.merge(v, v2)
        o.foreach { x => r = x }

        v() = 10

        enqueue { assert(r == 10) }

        v() = 15
        v2() = 20

        enqueue { assert(r == 20) }
      }

      'fromFuture{
        val p = Promise[Int]

        val o = Observable.from(p.future)
        o.foreach(x => r = x.get)

        enqueue { assert(r == 0) }

        enqueue { p.complete(Try(3)); () }

        enqueue { enqueue { assert(r == 3) } }
      }
    }

    'waitTillAllTimeoutsComplete{
      println((1 to 1000000).reduce(_ + _))
    }
  }

  def enqueue(block: => Unit): Unit = {
    dom.setTimeout(() => block, 0)
    ()
  }
}

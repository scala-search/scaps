package scaps

import scala.collection.mutable.ListBuffer
import scala.util.Random

package object utils {
  implicit class SampleSeqOps[T](s: Seq[T]) {
    def sample(n: Int, r: Random = Random): Seq[T] = {
      assert(n >= 0)

      val res = ListBuffer[T]()

      val length = s.length
      var samplesNeeded = n

      for { (e, i) <- s.zipWithIndex } {
        val p = samplesNeeded.toDouble / (length - i)

        if (p >= r.nextDouble()) {
          res += e
          samplesNeeded -= 1
        }
      }

      res.toSeq
    }
  }

  implicit class TraversableOps[A](t: TraversableOnce[A]) {
    import Ordering.Implicits._

    def minByOpt[B: Ordering](f: A => B) =
      t.reduceOption { (l, r) =>
        if (f(l) <= f(r))
          l
        else
          r
      }

    def maxByOpt[B: Ordering](f: A => B) =
      minByOpt(f)(implicitly[Ordering[B]].reverse)
  }
}

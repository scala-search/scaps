package scaps

package object nucleus {
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

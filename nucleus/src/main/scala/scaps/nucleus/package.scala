/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

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

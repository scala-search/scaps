/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.utils

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TraversalOpsSpecs extends FlatSpec with Matchers {
  "the traversal minByOpt extension" should "yield None on empty collections" in {
    List[Int]().minByOpt(identity) should be(None)
    Seq[Int]().minByOpt(identity) should be(None)
    Stream[Int]().minByOpt(identity) should be(None)
    Map[Int, String]().minByOpt(identity) should be(None)
  }

  it should "yield the min value on non empty collections" in {
    List(1).minByOpt(identity) should be(Some(1))
    Seq(1, 2).minByOpt(identity) should be(Some(1))
    Stream(Int.MinValue, Int.MaxValue).minByOpt(identity) should be(Some(Int.MinValue))
    Map(Int.MaxValue -> "a", Int.MinValue -> "b").minByOpt(identity) should be(Some(Int.MinValue -> "b"))
  }

  "the traversal maxByOpt extension" should "yield None on empty collections" in {
    List[Int]().maxByOpt(identity) should be(None)
    Seq[Int]().maxByOpt(identity) should be(None)
    Stream[Int]().maxByOpt(identity) should be(None)
    Map[Int, String]().maxByOpt(identity) should be(None)
  }

  it should "yield the max value on non empty collections" in {
    List(1).maxByOpt(identity) should be(Some(1))
    Seq(1, 2).maxByOpt(identity) should be(Some(2))
    Stream(Int.MinValue, Int.MaxValue).maxByOpt(identity) should be(Some(Int.MaxValue))
    Map(Int.MaxValue -> "a", Int.MinValue -> "b").maxByOpt(identity) should be(Some(Int.MaxValue -> "a"))
  }
}

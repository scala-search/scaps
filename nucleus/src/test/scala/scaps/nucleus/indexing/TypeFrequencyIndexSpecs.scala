/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.indexing

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.nucleus.Definition
import scaps.nucleus.IndexAccess
import scaps.nucleus.TestIndexAccess

class TypeFrequencyIndexSpecs extends FlatSpec with Matchers {
  import scaps.nucleus.TestLanguage._

  def tf(defs: List[Definition]) = {
    val index = new TestIndexAccess(defs)

    val tfs = TypeFrequencyIndex.typeFrequencies(index)

    assertElementaryRules(tfs)

    tfs
  }

  def assertElementaryRules(tfs: Map[String, Int]) = {
    val tfTop0 = tfs("-<top0>")
    val tfBottom0 = tfs("+<bottom0>")
    val tfUnknown0 = tfs("/<unknown0>")

    assert(tfs.filter(_._1.startsWith("-")).map(_._2).forall(_ <= tfTop0))
    assert(tfs.filter(_._1.startsWith("+")).map(_._2).forall(_ <= tfBottom0))
    assert(tfs.map(_._2).forall(_ <= tfUnknown0))
  }

  it should "collect frequencies at covariant positions" in {
    val defs = List(
      deff()(+T.Int),
      deff()(+T.Long),
      extendss(+T.Long),
      extendss(+T.Int, +T.Long))

    tf(defs) should contain allOf (
      "+Int" -> 2, "/Int" -> 1, "+Long" -> 1, "/Long" -> 1, "+<bottom0>" -> 2)
  }

  it should "collect frequencies at contravariant positions" in {
    val defs = List(
      deff(-T.Int)(+T.Unit),
      deff(-T.Long)(+T.Unit),
      extendss(+T.Long),
      extendss(+T.Int, +T.Long))

    tf(defs) should contain allOf (
      "-Int" -> 1, "/Int" -> 1, "-Long" -> 2, "/Long" -> 1, "-<top0>" -> 2)
  }

  it should "collect frequencies at invariant positions" in {
    val defs = List(
      deff(-T.Array(~T.Int))(+T.Unit),
      deff(-T.Array(~T.Long))(+T.Unit),
      extendss(+T.Long),
      extendss(+T.Int, +T.Long))

    tf(defs) should contain allOf (
      "/Int" -> 1, "/Long" -> 1, "/<unknown0>" -> 2)
  }
}

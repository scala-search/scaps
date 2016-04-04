/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ScapsSpecs extends FlatSpec with Matchers {
  import TestLanguage._

  class InMemoryIndex extends IndexAccess {
    var docs: List[Document] = Nil

    def add(newDocs: TraversableOnce[Document]): Unit = {
      docs = docs ++ newDocs
    }

    def getByKeys(keys: Seq[String]): Seq[Document] = {
      docs.filter(doc => keys.forall(k => doc.keys.contains(k)))
    }
  }

  it should "accept index jobs and type queries" in {
    val index = new InMemoryIndex
    val scaps = new Scaps(Settings(TestLanguage.testModel, IndexSettings.default, QuerySettings.default), index)

    val defs = Stream(
      TypeDef(Type(Nil, +T.Long), Nil, "long.t"),
      TypeDef(Type(Nil, +T.Int), +T.Long :: Nil, "int.t"),
      ValueDef("foo", Type(Nil, T.MethodInvocation(Covariant, List(), +T.Int)), false, "hello.t"),
      ValueDef("bar", Type(Nil, T.MethodInvocation(Covariant, List(), +T.Long)), false, "hello.t"))

    val indexBatch = scaps.startBatch()

    indexBatch.indexFile(defs)
    val indexedScaps = indexBatch.finalizeBatch()

    indexedScaps.search(Type(Nil, +T.Int)).map(_._1.name) should
      contain theSameElementsAs (List("foo"))

    indexedScaps.search(Type(Nil, +T.Long)).map(_._1.name) should
      contain theSameElementsAs (List("bar", "foo"))
  }
}

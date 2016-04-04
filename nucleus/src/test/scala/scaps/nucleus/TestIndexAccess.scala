/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus

import scaps.nucleus.indexing.Indexer

class TestIndexAccess(defs: Seq[Definition], language: LanguageSettings = TestLanguage.testModel) extends IndexAccess {
  val docs = defs.flatMap(Indexer.defToDocs(_, language))

  override def add(docs: TraversableOnce[Document]) = ()

  override def getByKeys(keys: Seq[String]) = {
    docs.filter(d => keys.forall(d.keys.contains(_)))
  }
}

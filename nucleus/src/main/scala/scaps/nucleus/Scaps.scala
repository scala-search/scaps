/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus

import scaps.nucleus.indexing.Indexer
import scaps.nucleus.indexing.InternalTypes
import scaps.nucleus.indexing.TypeFrequencyIndex
import scaps.nucleus.querying.QueryExpansion

trait IndexAccess {
  def getByKeys(keys: Seq[String]): Seq[Document]

  def getManyByKeys(keyss: Seq[Seq[String]]): Seq[Document] = keyss.flatMap(getByKeys)

  def countByKeys(keys: Seq[String]) = getByKeys(keys).size
}

class Scaps(settings: Settings) {
  def startBatch(): Batch = new Batch(settings)

  def createTermQuery(
    query: Type,
    index: IndexAccess): TermQuery = {
    ???
  }
}

class Batch private[nucleus] (settings: Settings) {

  def indexFile(source: String, definitions: Stream[Definition]): (Batch, Stream[Document]) =
    (this, definitions.flatMap(Indexer.defToDocs(_, settings.language)))

  def finalize(index: IndexAccess): (Scaps, TraversableOnce[Document]) = {
    (new Scaps(settings), TypeFrequencyIndex.typeFrequencyDocs(index))
  }
}

trait TermQuery {
  def keys: Iterator[Seq[String]]

  def score(doc: ValueDoc): Float
}

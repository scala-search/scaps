/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus

import scaps.nucleus.indexing.Indexer
import scaps.nucleus.indexing.InternalTypes
import scaps.nucleus.indexing.TypeFrequencyIndex
import scaps.nucleus.querying.QueryExpansion
import scaps.nucleus.indexing.TypeViewIndex
import scaps.nucleus.querying.QueryScorer
import scaps.nucleus.querying.QueryExpression
import scaps.nucleus.indexing.ValueIndex
import scaps.nucleus.indexing.Fingerprint

trait IndexAccess {
  def add(docs: TraversableOnce[Document]): Unit

  def getByKeys(keys: Seq[String]): Seq[Document]

  def getManyByKeys(keyss: Iterable[Seq[String]]): Seq[Document] = keyss.flatMap(getByKeys).toSeq

  def countByKeys(keys: Seq[String]) = getByKeys(keys).size
}

class Scaps(settings: Settings, index: IndexAccess) {
  def startBatch(): Batch = new Batch(settings, index)

  def search(query: Type, additionalMatches: Seq[(ValueDoc, Float)] = Nil): Seq[(ValueDoc, Float)] = {
    val queryExpression: QueryExpression = {
      val expanded = QueryExpansion.expandQuery(query, tpe => TypeViewIndex.viewsFrom(tpe, index))
      val getTf = TypeFrequencyIndex.relativeTermFrequency(index)_
      val scorer = new QueryScorer(settings.query, getTf)
      scorer.scoreQuery(expanded)
    }

    val keys = queryExpression.termsBelowCutoff(settings.query.fingerprintFrequencyCutoff)
      .map(term => Seq(term))

    (index.getManyByKeys(keys) ++ additionalMatches).collect {
      case v: ValueDoc =>
        v
    }.map { v =>
      val valueDef = ValueIndex.docToValue(v)
      (v, queryExpression.score(Fingerprint(valueDef))._1)
    }.sortBy(-_._2)
  }
}

class Batch private[nucleus] (settings: Settings, index: IndexAccess) {

  def indexFile(definitions: Stream[Definition]): Unit =
    index.add(definitions.flatMap(Indexer.defToDocs(_, settings.language)))

  def finalizeBatch(): Scaps = {
    index.add(TypeFrequencyIndex.typeFrequencyDocs(index))
    new Scaps(settings, index)
  }
}

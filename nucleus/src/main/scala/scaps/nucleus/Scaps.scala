package scaps.nucleus

import scaps.nucleus.indexing.Indexer
import scaps.nucleus.statistics.FrequencyAggregator
import scaps.nucleus.indexing.InternalTypes

trait IndexAccess {
  def getByKeys(keys: Seq[String]): Seq[Document]

  def getManyByKeys(keyss: Seq[Seq[String]]): Seq[Document] = keyss.flatMap(getByKeys)

  def countByKeys(keys: Seq[String]) = getByKeys(keys).size
}

class Scaps(settings: Settings) {
  def startBatch(): Batch = new Batch(settings)

  def createTermQuery(
    query: TypeQuery,
    index: IndexAccess): TermQuery = ???
}

class Batch private[nucleus] (settings: Settings) {

  def indexFile(source: String, definitions: Stream[Definition]): (Batch, Stream[Document]) = {
    (this, definitions
      .map(InternalTypes.toInternal(_, settings.language))
      .flatMap(d => Indexer.defToDocs(d)))
  }

  def finalize(index: IndexAccess): (Scaps, TraversableOnce[Document]) = {
    val aggregator = new FrequencyAggregator(settings.language, index)
    (new Scaps(settings), aggregator.typeFrequencyDocs())
  }
}

trait TermQuery {
  def keys: Iterator[Seq[String]]

  def score(doc: ValueDoc): Float
}

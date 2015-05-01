package scaps.searchEngine.index

import java.io.Reader
import scala.collection.JavaConversions.mapAsJavaMap
import scaps.webapi.TermEntity
import scaps.searchEngine.APIQuery
import scaps.searchEngine.ProcessingError
import scaps.searchEngine.TooUnspecific
import scaps.settings.Settings
import scala.util.Try
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.LowerCaseFilter
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper
import org.apache.lucene.analysis.miscellaneous.WordDelimiterFilter
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.FieldInvertState
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.MatchAllDocsQuery
import org.apache.lucene.search.Query
import org.apache.lucene.search.TermQuery
import org.apache.lucene.search.similarities.DefaultSimilarity
import org.apache.lucene.search.similarities.PerFieldSimilarityWrapper
import org.apache.lucene.search.similarities.TFIDFSimilarity
import org.apache.lucene.store.Directory
import org.apache.lucene.util.BytesRef
import scalaz.{ \/ => \/ }
import scalaz.syntax.either.ToEitherOps
import scaps.webapi.Module
import org.apache.lucene.analysis.core.KeywordAnalyzer

class TermsIndex(val dir: Directory, settings: Settings) extends Index[TermEntity] {
  import TermsIndex._

  override val analyzer =
    new PerFieldAnalyzerWrapper(new StandardAnalyzer(), Map(
      fields.fingerprint -> new WhitespaceAnalyzer,
      fields.moduleId -> new KeywordAnalyzer,
      fields.name -> new Analyzer {
        override def createComponents(fieldName: String, reader: Reader) = {
          val tokenizer = new WhitespaceTokenizer(reader)
          val ts1 = new WordDelimiterFilter(
            tokenizer,
            WordDelimiterFilter.GENERATE_WORD_PARTS |
              WordDelimiterFilter.GENERATE_NUMBER_PARTS |
              WordDelimiterFilter.PRESERVE_ORIGINAL |
              WordDelimiterFilter.SPLIT_ON_CASE_CHANGE |
              WordDelimiterFilter.SPLIT_ON_NUMERICS,
            null)
          val ts2 = new LowerCaseFilter(ts1)
          new TokenStreamComponents(tokenizer, ts2)
        }
      }))

  override val similarity = new PerFieldSimilarityWrapper {
    val default = new DefaultSimilarity

    override def get(field: String) = field match {
      case fields.fingerprint => new FingerprintSimilarity(settings)
      case _                  => default
    }
  }

  def find(query: APIQuery, moduleIds: Set[String]): Try[ProcessingError \/ Seq[TermEntity]] =
    Try {
      toLuceneQuery(query, moduleIds).map(
        lq => search(lq, settings.query.maxResults).get)
    }

  def deleteEntitiesIn(module: Module): Try[Unit] =
    withWriter { writer =>
      writer.deleteDocuments(new Term(fields.moduleId, module.moduleId))
    }

  private def toLuceneQuery(query: APIQuery, moduleIds: Set[String]): ProcessingError \/ Query = {
    try {
      val q = new BooleanQuery

      query.keywords.foreach { keyword =>
        val nameQuery = new TermQuery(new Term(fields.name, keyword))
        nameQuery.setBoost(settings.query.nameBoost.toFloat)
        q.add(nameQuery, Occur.SHOULD)

        val docQuery = new TermQuery(new Term(fields.doc, keyword))
        docQuery.setBoost(settings.query.docBoost.toFloat)
        q.add(docQuery, Occur.SHOULD)
      }

      query.types.foreach { tpe =>
        val fingerprint = s"${tpe.variance.prefix}${tpe.typeName}_${tpe.occurrence}"
        val tq = new TermQuery(new Term(fields.fingerprint, fingerprint))
        tq.setBoost(tpe.boost)
        q.add(tq, Occur.SHOULD)
      }

      if (!moduleIds.isEmpty) {
        val moduleQuery = new BooleanQuery
        moduleIds.foreach { moduleId =>
          val tq = new TermQuery(new Term(fields.moduleId, moduleId))
          moduleQuery.add(tq, Occur.SHOULD)
        }
        q.add(moduleQuery, Occur.MUST)
      }

      q.right
    } catch {
      case _: BooleanQuery.TooManyClauses => TooUnspecific().left
    }
  }

  def allTerms(): Try[Seq[TermEntity]] =
    search(new MatchAllDocsQuery)

  private[index] def findTermsByName(name: String): Try[Seq[TermEntity]] =
    search(new TermQuery(new Term(fields.name, name)))

  override def toDocument(entity: TermEntity): Document = {
    val doc = new Document

    def add(field: String, value: String) =
      doc.add(new TextField(field, value, Store.YES))

    add(fields.name, entity.name)
    add(fields.moduleId, entity.module.moduleId)
    entity.fingerprint.bagOfTypes.foreach { tpe =>
      add(fields.fingerprint, tpe)
    }
    add(fields.doc, entity.comment)
    doc.add(new StoredField(fields.entity, upickle.write(entity)))

    doc
  }

  override def toEntity(doc: Document): TermEntity = {
    val json = doc.getValues(fields.entity)(0)

    upickle.read[TermEntity](json)
  }
}

object TermsIndex {
  object fields {
    val name = "name"
    val fingerprint = "fingerprint"
    val doc = "doc"
    val entity = "entity"
    val moduleId = "moduleId"
  }

  class FingerprintSimilarity(settings: Settings) extends TFIDFSimilarity {
    val delegate = new DefaultSimilarity

    // Reduce influence of IDF in order to cope with missing reflection
    // of type hierarchies in doc frequencies
    override def idf(docFreq: Long, numDocs: Long) =
      settings.query.idfWeight.toFloat * (delegate.idf(docFreq, numDocs) - 1f) + 1f

    // Override decoding to represent shorter documents with higher precision
    val prec = Long.MaxValue.toFloat
    override def decodeNormValue(b: Long): Float = (b.toFloat) / prec
    override def encodeNormValue(f: Float): Long = (f * prec).toLong

    override def lengthNorm(state: FieldInvertState): Float =
      settings.index.lengthNormWeight.toFloat * (delegate.lengthNorm(state) - 1f) + 1f

    // delegate remaining methods to default similarity
    override def scorePayload(doc: Int, start: Int, end: Int, payload: BytesRef): Float = delegate.scorePayload(doc, start, end, payload)
    override def sloppyFreq(distance: Int): Float = delegate.sloppyFreq(distance)
    override def tf(freq: Float): Float = delegate.tf(freq)
    override def queryNorm(sumOfSquaredWeights: Float): Float = delegate.queryNorm(sumOfSquaredWeights)
    override def coord(overlap: Int, maxOverlap: Int): Float = delegate.coord(overlap, maxOverlap)
  }
}

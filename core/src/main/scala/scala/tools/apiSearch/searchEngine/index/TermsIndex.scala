package scala.tools.apiSearch.searchEngine.index

import java.io.Reader

import scala.collection.JavaConversions.mapAsJavaMap
import scala.tools.apiSearch.model.TermEntity
import scala.tools.apiSearch.searchEngine.APIQuery
import scala.tools.apiSearch.settings.Settings
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
import org.apache.lucene.store.Directory

class TermsIndex(val dir: Directory, settings: Settings) extends Index[TermEntity] {
  import TermsIndex._

  override val analyzer =
    new PerFieldAnalyzerWrapper(new StandardAnalyzer(), Map(
      fields.fingerprint -> new WhitespaceAnalyzer,
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
      case fields.fingerprint => new DefaultSimilarity {
        // Reduce influence of IDF in order to cope with missing reflection
        // of type hierarchies in doc frequencies
        override def idf(a: Long, b: Long) =
          settings.query.idfWeight.toFloat * (super.idf(a, b) - 1f) + 1f

        // default length norm is `boost * (1/sqrt(length))` but we use a steeper function
        // because fingerprints are relatively short documents
        override def lengthNorm(state: FieldInvertState): Float =
          state.getBoost * (1f / Math.sqrt(settings.index.lengthNormWeight * state.getLength).toFloat)
      }
      case _ => default
    }
  }

  def find(query: APIQuery): Try[Seq[TermEntity]] =
    search(toLuceneQuery(query), settings.query.maxResults)

  private def toLuceneQuery(query: APIQuery): Query = {
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
    q
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
    add(fields.fingerprint, entity.fingerprint)
    add(fields.doc, entity.comment)
    doc.add(new StoredField(fields.entity, Serialization.pickle(entity)))

    doc
  }

  override def toEntity(doc: Document): TermEntity = {
    val bytes = doc.getBinaryValues(fields.entity).flatMap(_.bytes)

    Serialization.unpickleTerm(bytes)
  }
}

object TermsIndex {
  object fields {
    val name = "name"
    val fingerprint = "fingerprint"
    val doc = "doc"
    val entity = "entity"
  }
}

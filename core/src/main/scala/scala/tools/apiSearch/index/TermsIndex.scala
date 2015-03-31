package scala.tools.apiSearch.index

import java.io.Reader

import scala.collection.JavaConversions.mapAsJavaMap
import scala.collection.JavaConversions.seqAsJavaList
import scala.tools.apiSearch.model.TermEntity
import scala.tools.apiSearch.searching.APIQuery
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
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.Query
import org.apache.lucene.search.TermQuery
import org.apache.lucene.search.similarities.DefaultSimilarity
import org.apache.lucene.search.similarities.PerFieldSimilarityWrapper
import org.apache.lucene.store.Directory

class TermsIndex(val dir: Directory) extends Index {
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
        override def idf(a: Long, b: Long) = 1

        // default length norm is `boost * (1/sqrt(length))` but we use a steeper function
        // because fingerprints are relatively short documents
        //        override def lengthNorm(state: FieldInvertState): Float =
        //          state.getBoost * (1f / Math.sqrt(2 * state.getLength))
      }
      case _ => default
    }
  }

  /**
   * Adds all entities to the index.
   */
  def addEntities(entities: Seq[TermEntity]): Try[Unit] =
    withWriter { writer =>
      val docs = entities.map(toDocument)
      Try(writer.addDocuments(docs))
    }

  def find(query: APIQuery): Try[Seq[TermEntity]] =
    withSearcher { searcher =>
      val docs = searcher.search(toLuceneQuery(query), maxResults)

      docs.scoreDocs.map(scoreDoc =>
        toTermEntity(searcher.doc(scoreDoc.doc)))
    }

  private def toLuceneQuery(query: APIQuery): Query = {
    val q = new BooleanQuery
    query.keywords.foreach { keyword =>
      q.add(new TermQuery(new Term(fields.name, keyword)), Occur.SHOULD)
      q.add(new TermQuery(new Term(fields.doc, keyword)), Occur.SHOULD)
    }
    query.types.foreach { tpe =>
      val fingerprint = s"${tpe.variance.prefix}${tpe.typeName}_${tpe.occurrence}"
      val tq = new TermQuery(new Term(fields.fingerprint, fingerprint))
      tq.setBoost(tpe.boost)
      q.add(tq, Occur.SHOULD)
    }
    q
  }

  /**
   * Searches for term entities whose name matches `name`.
   */
  def findTermsByName(name: String): Try[Seq[TermEntity]] = {
    withSearcher { searcher =>
      val query = new TermQuery(new Term(fields.name, name))

      val docs = searcher.search(query, maxResults)

      docs.scoreDocs.map(scoreDoc =>
        toTermEntity(searcher.doc(scoreDoc.doc)))
    }
  }

  private def toDocument(entity: TermEntity): Document = {
    val doc = new Document

    def add(field: String, value: String) =
      doc.add(new TextField(field, value, Store.YES))

    add(fields.name, entity.name)
    add(fields.fingerprint, entity.fingerprint)
    add(fields.doc, entity.comment)
    doc.add(new StoredField(fields.entity, Serialization.pickle(entity)))

    doc
  }

  private def toTermEntity(doc: Document): TermEntity = {
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

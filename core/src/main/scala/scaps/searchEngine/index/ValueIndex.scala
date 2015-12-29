package scaps.searchEngine.index

import java.io.Reader
import scala.collection.JavaConverters.mapAsJavaMapConverter
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.util.Try
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.analysis.core.LowerCaseFilter
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.analysis.en.KStemFilter
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper
import org.apache.lucene.analysis.miscellaneous.WordDelimiterFilter
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.analysis.util.CharTokenizer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.Query
import org.apache.lucene.search.similarities.DefaultSimilarity
import org.apache.lucene.search.similarities.PerFieldSimilarityWrapper
import org.apache.lucene.store.Directory
import org.apache.lucene.util.QueryBuilder
import scalaz.Scalaz._
import scalaz.{ \/ => \/ }
import scalaz.syntax.either.ToEitherOps
import scaps.api.Module
import scaps.api.Result
import scaps.api.ValueDef
import scaps.searchEngine.ApiQuery
import scaps.searchEngine.ProcessingError
import scaps.searchEngine.TooUnspecific
import scaps.settings.Settings
import org.apache.lucene.index.FieldInvertState
import scaps.api.Invariant

class ValueIndex(val dir: Directory, settings: Settings) extends Index[ValueDef] {
  import ValueIndex._

  private val nameAnalyzer = new Analyzer {
    override def createComponents(fieldName: String, reader: Reader) = {
      val tokenizer = new CharTokenizer(reader) {
        override def isTokenChar(c: Int): Boolean = {
          !(Character.isWhitespace(c) || c == '.')
        }
      }

      val ts1 = new WordDelimiterFilter(
        tokenizer,
        WordDelimiterFilter.GENERATE_WORD_PARTS |
          WordDelimiterFilter.GENERATE_NUMBER_PARTS |
          WordDelimiterFilter.SPLIT_ON_CASE_CHANGE |
          WordDelimiterFilter.SPLIT_ON_NUMERICS |
          WordDelimiterFilter.PRESERVE_ORIGINAL,
        null)
      val ts2 = new LowerCaseFilter(ts1)
      val ts3 = new KStemFilter(ts2)
      new TokenStreamComponents(tokenizer, ts3)
    }
  }

  private val docAnalyzer = nameAnalyzer

  override val analyzer =
    new PerFieldAnalyzerWrapper(new StandardAnalyzer(), Map(
      fields.fingerprintTerms -> new WhitespaceAnalyzer,
      fields.moduleId -> new KeywordAnalyzer,
      fields.name -> nameAnalyzer,
      fields.doc -> docAnalyzer).asJava)

  private val queryBuilder = new QueryBuilder(analyzer)

  override val similarity = new PerFieldSimilarityWrapper {
    val default = new DefaultSimilarity {
      override def lengthNorm(state: FieldInvertState): Float = {
        val numTerms =
          if (discountOverlaps)
            state.getLength - state.getNumOverlap
          else
            state.getLength

        state.getBoost * (1.0 / math.pow(numTerms + 10, 0.25)).toFloat
      }
    }

    override def get(field: String) = field match {
      case fields.moduleId => new DefaultSimilarity {
        override def idf(docFreq: Long, numDocs: Long) = 1f
      }
      case _ => default
    }
  }

  def addEntities(entities: Seq[ValueDef]): Try[Unit] =
    withWriter { writer =>
      val docs = entities.map(toDocument)
      writer.addDocuments(docs.asJava)
    }

  def find(query: ApiQuery, moduleIds: Set[String]): Try[ProcessingError \/ Seq[Result[ValueDef]]] =
    Try {
      toLuceneQuery(query, moduleIds).map(
        lq => search(lq, settings.query.maxResults, settings.query.explainScores).get)
    }

  def deleteEntitiesIn(module: Module): Try[Unit] =
    withWriter { writer =>
      writer.deleteDocuments(new Term(fields.moduleId, module.moduleId))
    }

  private def toLuceneQuery(query: ApiQuery, moduleIds: Set[String]): ProcessingError \/ Query = {
    try {
      val keys = new BooleanQuery

      Option(queryBuilder.createBooleanQuery(fields.doc, query.keywords))
        .foreach { docQuery =>
          keys.add(docQuery, Occur.SHOULD)
        }

      val keysAndTypes = query.tpe.fold[Query] {
        keys
      } { tpeQuery =>
        new TypeFingerprintQuery(fields.fingerprintTerms, fields.fingerprint, tpeQuery, keys, settings.query)
      }

      val q = new BooleanQuery
      q.add(keysAndTypes, Occur.MUST)
      q.add(Index.moduleQuery(moduleIds, fields.moduleId), Occur.MUST)

      q.right
    } catch {
      case _: BooleanQuery.TooManyClauses => TooUnspecific.left
    }
  }

  private[index] def findValuesByName(name: String): Try[Seq[ValueDef]] =
    search(queryBuilder.createBooleanQuery(fields.name, name)).map(_.map(_.entity))

  def toDocument(entity: ValueDef): Document = {
    val doc = new Document

    doc.add(new TextField(fields.name, entity.name, Store.NO))

    doc.add(new TextField(fields.doc, (entity.name + "\n").multiply(2) + entity.comment.indexableContent, Store.NO))
    doc.add(new TextField(fields.moduleId, entity.module.moduleId, Store.NO))

    val fingerprint = Fingerprint(entity)
    fingerprint.termsWithIsOpt.foreach {
      case (term, _) =>
        doc.add(new TextField(fields.fingerprintTerms, term, Store.NO))
    }
    doc.add(new TextField(fields.fingerprint, fingerprint.toString(), Store.YES))

    doc.add(new StoredField(fields.entity, upickle.write(entity)))

    doc
  }

  override def toEntity(doc: Document): ValueDef = {
    val json = doc.getValues(fields.entity)(0)

    upickle.read[ValueDef](json)
  }
}

object ValueIndex {
  object fields {
    val name = "name"
    val fingerprintTerms = "fingerprintTerms"
    val fingerprint = "fingerprint"
    val doc = "doc"
    val entity = "entity"
    val moduleId = "moduleId"
  }
}

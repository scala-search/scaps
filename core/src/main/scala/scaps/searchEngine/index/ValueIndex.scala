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
import org.apache.lucene.document.NumericDocValuesField
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.Term
import org.apache.lucene.queries.function.FunctionQuery
import org.apache.lucene.queries.function.FunctionValues
import org.apache.lucene.queries.function.valuesource.IntFieldSource
import org.apache.lucene.queries.function.valuesource.SimpleFloatFunction
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.MatchAllDocsQuery
import org.apache.lucene.search.Query
import org.apache.lucene.search.TermQuery
import org.apache.lucene.search.similarities.DefaultSimilarity
import org.apache.lucene.search.similarities.PerFieldSimilarityWrapper
import org.apache.lucene.store.Directory
import org.apache.lucene.util.QueryBuilder

import scalaz.{ \/ => \/ }
import scalaz.syntax.either.ToEitherOps
import scaps.searchEngine.ApiQuery
import scaps.searchEngine.ProcessingError
import scaps.searchEngine.TooUnspecific
import scaps.settings.Settings
import scaps.api.Module
import scaps.api.ValueDef

class ValueIndex(val dir: Directory, settings: Settings) extends Index[ValueDef] {
  import ValueIndex._

  private val nameAnalyzer = new Analyzer {
    override def createComponents(fieldName: String, reader: Reader) = {
      val tokenizer = new IdentifierAndWhitespaceDelimiterTokenizer(reader)
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
      fields.fingerprint -> new WhitespaceAnalyzer,
      fields.moduleId -> new KeywordAnalyzer,
      fields.name -> nameAnalyzer,
      fields.doc -> docAnalyzer).asJava)

  private val queryBuilder = new QueryBuilder(analyzer)

  override val similarity = new PerFieldSimilarityWrapper {
    val default = new DefaultSimilarity

    override def get(field: String) = field match {
      case fields.moduleId => new ModuleIdSimilarity()
      case _               => default
    }
  }

  def addEntities(entities: Seq[ValueDef]): Try[Unit] =
    withWriter { writer =>
      val docs = entities.map(toDocument)
      writer.addDocuments(docs.asJava)
    }

  def find(query: ApiQuery, moduleIds: Set[String]): Try[ProcessingError \/ Seq[ValueDef]] =
    Try {
      //      toLuceneQuery(query, moduleIds).map(
      //        lq => search(lq, 6, Some((value, expl) => println(s"${value.withoutComment}\n${value.typeFingerprint}\n$expl"))).get)
      toLuceneQuery(query, moduleIds).map(
        lq => search(lq, settings.query.maxResults).get)
    }

  def deleteEntitiesIn(module: Module): Try[Unit] =
    withWriter { writer =>
      writer.deleteDocuments(new Term(fields.moduleId, module.moduleId))
    }

  private def toLuceneQuery(query: ApiQuery, moduleIds: Set[String]): ProcessingError \/ Query = {
    try {
      val keys = new BooleanQuery

      Option(queryBuilder.createBooleanQuery(fields.name, query.keywords))
        .foreach { nameQuery =>
          nameQuery.setBoost(settings.query.nameBoost.toFloat)
          keys.add(nameQuery, Occur.SHOULD)
        }

      Option(queryBuilder.createBooleanQuery(fields.doc, query.keywords))
        .foreach { docQuery =>
          docQuery.setBoost(settings.query.docBoost.toFloat)
          keys.add(docQuery, Occur.SHOULD)
        }

      def docLenBoost = new FunctionQuery(
        new SimpleFloatFunction(new IntFieldSource(fields.fingerprintLength)) {
          val lengthWeight = settings.query.lengthNormWeight / math.sqrt(query.queryFingerprintLength)

          def func(doc: Int, fingerprintLengthValues: FunctionValues): Float = {
            val fingerprintLength = fingerprintLengthValues.intVal(doc)
            (1d /
              (math.pow(
                lengthWeight * math.abs(query.queryFingerprintLength - fingerprintLength),
                2) + 1)).toFloat
          }

          def name(): String = "docLenNormalization"
        })

      val keysAndTypes = query.tpe.fold[Query] {
        keys
      } { tpe =>
        new TypeFingerprintQuery(
          fields.fingerprint, tpe, keys, settings.query.fingerprintFrequencyCutoff, docLenBoost)
      }

      val q = new BooleanQuery
      q.add(keysAndTypes, Occur.MUST)
      q.add(Index.moduleQuery(moduleIds, fields.moduleId), Occur.MUST)

      q.right
    } catch {
      case _: BooleanQuery.TooManyClauses => TooUnspecific().left
    }
  }

  private[index] def findValuesByName(name: String): Try[Seq[ValueDef]] =
    search(queryBuilder.createBooleanQuery(fields.name, name))

  override def toDocument(entity: ValueDef): Document = {
    val doc = new Document

    doc.add(new TextField(fields.name, entity.name, Store.NO))

    doc.add(new TextField(fields.doc, entity.comment, Store.NO))
    doc.add(new TextField(fields.moduleId, entity.module.moduleId, Store.NO))

    entity.typeFingerprint.foreach { fp =>
      doc.add(new TextField(fields.fingerprint, fp, Store.YES))
    }

    doc.add(new StoredField(fields.entity, upickle.write(entity)))
    doc.add(new NumericDocValuesField(fields.fingerprintLength, entity.tpe.toList.length))

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
    val fingerprint = "fingerprint"
    val doc = "doc"
    val entity = "entity"
    val moduleId = "moduleId"
    val fingerprintLength = "fingerprintLength"
  }

  class ModuleIdSimilarity extends DefaultSimilarity {
    override def idf(docFreq: Long, numDocs: Long) = 1f
  }

  class IdentifierAndWhitespaceDelimiterTokenizer(reader: Reader) extends CharTokenizer(reader) {
    override def isTokenChar(c: Int): Boolean = {
      !(Character.isWhitespace(c) || c == '.')
    }
  }
}

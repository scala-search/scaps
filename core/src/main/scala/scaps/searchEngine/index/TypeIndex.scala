package scaps.searchEngine.index

import scala.util.Try
import scala.collection.JavaConverters._
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.MatchAllDocsQuery
import org.apache.lucene.search.TermQuery
import org.apache.lucene.store.Directory
import scaps.settings.Settings
import scaps.api.TypeDef
import scaps.api.Covariant
import scaps.api.Module
import scaps.api.TypeRef
import scala.annotation.tailrec
import scaps.api.Variance
import org.apache.lucene.search.DocIdSetIterator
import scaps.api.Contravariant
import scaps.api.Invariant

/**
 * Persists class entities and provides lookup for typeDefs by name.
 *
 * This index is mainly used for fast access to class hierarchies for query building.
 */
class TypeIndex(val dir: Directory, settings: Settings) extends Index[TypeDef] {
  import TypeIndex._

  val analyzer = new KeywordAnalyzer

  def addEntities(entities: Seq[TypeDef]): Try[Unit] =
    withWriter { writer =>
      entities.foreach { e =>
        val idTerm = new Term(fields.id, id(e))
        val doc = toDocument(e)
        writer.updateDocument(idTerm, doc)
      }
    }

  def deleteEntitiesIn(module: Module): Try[Unit] =
    withWriter { writer =>
      writer.deleteDocuments(new Term(fields.moduleId, module.moduleId))
    }

  /**
   * Searches for class entities whose last parts of the full qualified name are `suffix`
   * and accept `noArgs` type parameters.
   */
  def findTypeDefsBySuffix(suffix: String, moduleIds: Set[String] = Set()): Try[Seq[TypeDef]] = {
    val query = new BooleanQuery()

    query.add(new TermQuery(new Term(fields.suffix, suffix)), Occur.MUST)
    query.add(Index.moduleQuery(moduleIds, fields.moduleId), Occur.MUST)

    search(query).map(_.map(_.withModule(Module.Unknown)).distinct)
  }

  def findTypeDef(name: String): Try[Option[TypeDef]] = Try {
    search(new TermQuery(new Term(fields.name, name))).get.headOption
  }

  def allTypeDefs(): Try[Seq[TypeDef]] =
    search(new MatchAllDocsQuery)

  def updateTypeFrequencies(tfs: Map[(Variance, String), Float]): Try[Unit] =
    Try {
      val updated = withSearcher { searcher =>
        val docs = searcher.search(new MatchAllDocsQuery, Int.MaxValue)

        for {
          i <- 0 until docs.scoreDocs.length
          docId = docs.scoreDocs(i).doc
          if docId != DocIdSetIterator.NO_MORE_DOCS
        } yield {
          val doc = searcher.doc(docId)
          val tpe = toEntity(doc)

          def freq(v: Variance) = tfs((v, tpe.name))

          tpe.copy(typeFrequency = Map(
            Covariant -> freq(Covariant),
            Contravariant -> freq(Contravariant),
            Invariant -> freq(Invariant)))
        }
      }.get

      replaceAllEntities(updated).get
    }

  private def replaceAllEntities(entities: Seq[TypeDef]): Try[Unit] =
    withWriter { writer =>
      val docs = entities.map(toDocument)
      writer.deleteAll()
      writer.addDocuments(docs.asJavaCollection)
    }

  def toDocument(entity: TypeDef): Document = {
    val doc = new Document

    doc.add(new TextField(fields.name, entity.name, Field.Store.YES))

    for (suffix <- suffixes(entity.name)) {
      doc.add(new TextField(fields.suffix, suffix, Field.Store.NO))
    }

    doc.add(new StoredField(fields.entity, upickle.write(entity)))

    doc.add(new TextField(fields.moduleId, entity.module.moduleId, Field.Store.YES))

    doc
  }

  private def id(t: TypeDef) =
    s"${t.module.moduleId}:${t.name}"

  private def suffixes(name: String): List[String] = name match {
    case "" => Nil
    case s =>
      s.indexWhere(c => c == '.' || c == '#') match {
        case -1  => s :: Nil
        case idx => s :: suffixes(name.drop(idx + 1))
      }
  }

  override def toEntity(doc: Document): TypeDef = {
    val json = doc.getValues(fields.entity)(0)

    upickle.read[TypeDef](json)
  }
}

object TypeIndex {
  object fields {
    val id = "id"
    val name = "name"
    val suffix = "suffix"
    val entity = "entity"
    val moduleId = "moduleId"
  }
}

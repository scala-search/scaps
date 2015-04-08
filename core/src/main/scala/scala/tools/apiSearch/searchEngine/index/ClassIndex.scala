package scala.tools.apiSearch.searchEngine.index

import scala.tools.apiSearch.model.ClassEntity
import scala.tools.apiSearch.settings.Settings
import scala.util.Try
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.TermQuery
import org.apache.lucene.store.Directory
import org.apache.lucene.search.MatchAllDocsQuery

/**
 * Persists class entities and provides lookup for classes by name.
 *
 * This index is mainly used for fast access to class hierarchies for query building.
 */
class ClassIndex(val dir: Directory, settings: Settings) extends Index {
  import ClassIndex._

  val analyzer = new WhitespaceAnalyzer

  /**
   * Adds all entities to the index.
   */
  def addEntities(entities: Seq[ClassEntity]): Try[Unit] = {
    withWriter { writer =>
      entities.foreach { entity =>
        val doc = toDocument(entity)
        writer.updateDocument(new Term(fields.name, entity.name), doc)
      }
    }
  }

  /**
   * Searches for the class entity whose name matches `name`.
   */
  def findClassByName(name: String): Try[Option[ClassEntity]] = {
    withSearcher { searcher =>
      val query = new TermQuery(new Term(fields.name, name))

      val docs = searcher.search(query, 1)

      docs.scoreDocs.headOption.map(scoreDoc =>
        toClassEntity(searcher.doc(scoreDoc.doc)))
    }
  }

  /**
   * Searches for class entities whose last parts of the full qualified name are `suffix`
   * and accept `noArgs` type parameters.
   */
  def findClass(suffix: String): Try[Seq[ClassEntity]] = {
    withSearcher { searcher =>
      val query = new BooleanQuery()
      query.add(new TermQuery(new Term(fields.suffix, suffix)), Occur.MUST)

      val docs = searcher.search(query, settings.query.maxResults)

      docs.scoreDocs.map(scoreDoc =>
        toClassEntity(searcher.doc(scoreDoc.doc)))
    }
  }

  def findSubClasses(cls: ClassEntity): Try[Seq[ClassEntity]] = {
    withSearcher { searcher =>
      val query = new TermQuery(new Term(fields.baseClass, cls.name))

      val docs = searcher.search(query, settings.query.maxResults)

      docs.scoreDocs.map(scoreDoc =>
        toClassEntity(searcher.doc(scoreDoc.doc)))
    }
  }

  def allClasses(): Try[Seq[ClassEntity]] = {
    withSearcher { searcher =>
      val docs = searcher.search(new MatchAllDocsQuery, Int.MaxValue)

      docs.scoreDocs.map(scoreDoc =>
        toClassEntity(searcher.doc(scoreDoc.doc)))
    }
  }

  private def toDocument(entity: ClassEntity): Document = {
    val doc = new Document

    doc.add(new TextField(fields.name, entity.name, Field.Store.YES))
    for (suffix <- suffixes(entity.name)) {
      doc.add(new TextField(fields.suffix, suffix, Field.Store.NO))
    }
    for (baseClass <- entity.baseTypes) {
      doc.add(new TextField(fields.baseClass, baseClass.name, Field.Store.NO))
    }
    doc.add(new StoredField(fields.entity, Serialization.pickle(entity)))

    doc
  }

  private def suffixes(name: String): List[String] = name match {
    case "" => Nil
    case s =>
      s.indexWhere(c => c == '.' || c == '#') match {
        case -1  => s :: Nil
        case idx => s :: suffixes(name.drop(idx + 1))
      }
  }

  private def toClassEntity(doc: Document): ClassEntity = {
    val bytes = doc.getBinaryValues(fields.entity).flatMap(_.bytes)

    Serialization.unpickleClass(bytes)
  }
}

object ClassIndex {
  object fields {
    val name = "name"
    val suffix = "suffix"
    val baseClass = "baseClass"
    val entity = "entity"
  }
}
package scala.tools.apiSearch.index

import scala.collection.JavaConversions._
import scala.tools.apiSearch.model.ClassEntity
import scala.tools.apiSearch.utils.using
import scala.util.Try
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.Term
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.TermQuery
import org.apache.lucene.store.Directory
import scala.tools.apiSearch.model.ClassEntity
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.BooleanClause.Occur

/**
 * Persists class entities and provides lookup for classes by name.
 *
 * This index is mainly used for fast access to class hierarchies for query building.
 */
class ClassIndex(val dir: Directory) extends Index {
  import ClassIndex._

  val analyzer = new WhitespaceAnalyzer

  /**
   * Adds all entities to the index.
   */
  def addEntities(entities: Seq[ClassEntity]): Try[Unit] = {
    withWriter { writer =>
      val docs = entities.map(toDocument)
      Try(writer.addDocuments(docs))
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
  def findClass(suffix: String, noArgs: Int): Try[Seq[ClassEntity]] = {
    withSearcher { searcher =>
      val query = new BooleanQuery()
      query.add(new TermQuery(new Term(fields.suffix, suffix)), Occur.MUST)
      query.add(new TermQuery(new Term(fields.noParams, noArgs.toString)), Occur.MUST)

      val docs = searcher.search(query, maxResults)

      docs.scoreDocs.map(scoreDoc =>
        toClassEntity(searcher.doc(scoreDoc.doc)))
    }
  }

  def findSubClasses(clsName: String): Try[Seq[ClassEntity]] = {
    withSearcher { searcher =>
      val query = new TermQuery(new Term(fields.baseClass, clsName))

      val docs = searcher.search(query, maxResults)

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
    doc.add(new TextField(fields.noParams, entity.typeParameters.length.toString, Field.Store.YES))
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
    val noParams = "noParams"
    val baseClass = "baseClass"
    val entity = "entity"
  }
}

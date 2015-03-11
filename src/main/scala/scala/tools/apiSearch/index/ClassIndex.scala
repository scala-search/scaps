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

class ClassIndex(dir: Directory) {
  import ClassIndex._

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
        toTermEntity(searcher.doc(scoreDoc.doc)))
    }
  }

  private def toDocument(entity: ClassEntity): Document = {
    val doc = new Document

    doc.add(new TextField(fields.name, entity.name, Field.Store.YES))
    doc.add(new StoredField(fields.entity, Serialization.pickle(entity)))

    doc
  }

  private def toTermEntity(doc: Document): ClassEntity = {
    val bytes = doc.getBinaryValues(fields.entity).flatMap(_.bytes)

    Serialization.unpickleClass(bytes)
  }

  private def withWriter[A](f: IndexWriter => A): Try[A] = {
    val writerConf = new IndexWriterConfig(analyzer)

    using(new IndexWriter(dir, writerConf)) { w =>
      Try(f(w))
    }
  }

  private def withSearcher[A](f: IndexSearcher => A): Try[A] = {
    using(DirectoryReader.open(dir)) { reader =>
      Try(f(new IndexSearcher(reader)))
    }
  }

  private val analyzer =
    new PerFieldAnalyzerWrapper(new StandardAnalyzer(), Map(
      fields.name -> new WhitespaceAnalyzer))
}

object ClassIndex {
  object fields {
    val name = "name"
    val entity = "entity"
  }
}

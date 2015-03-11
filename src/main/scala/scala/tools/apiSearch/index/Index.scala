package scala.tools.apiSearch.index

import scala.collection.JavaConversions._
import scala.tools.apiSearch.model._
import scala.util.Try
import org.apache.lucene.document.Document
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.store.Directory
import org.apache.lucene.index.IndexWriterConfig
import scala.tools.apiSearch.utils.using
import org.apache.lucene.document.Field
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.util.Version
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.document.TextField
import org.apache.lucene.document.Field.Store

class Index(indexDir: Directory) {
  /**
   * Adds all entities to the index.
   */
  def addEntities(entities: Seq[Entity]): Try[Unit] = {
    withWriter { writer =>
      val docs = entities.map(toDocument)
      Try(writer.addDocuments(docs))
    }
  }

  /**
   * Searches for term entities matching `keywords` and `tpe` if defined.
   */
  def findTerms(keywords: Seq[String], tpe: Option[TypeEntity]): Seq[TermEntity] = ???

  private def toDocument(entity: Entity): Document = {
    val doc = new Document

    def add(field: String, value: String) =
      doc.add(new TextField(field, value, Store.YES))

    add(fields.name, entity.name)

    entity match {
      case term: TermEntity =>
        add(fields.fingerprint, term.fingerprint)
      case cls: ClassEntity =>
        ()
    }

    doc
  }

  private def withWriter[A](f: IndexWriter => A): Try[A] = {
    val writerConf = new IndexWriterConfig(analyzer)

    using(new IndexWriter(indexDir, writerConf)) { w =>
      Try(f(w))
    }
  }

  private val analyzer =
    new PerFieldAnalyzerWrapper(new StandardAnalyzer(), Map(
      fields.fingerprint -> new WhitespaceAnalyzer))

  object fields {
    val name = "name"
    val fingerprint = "fingerprint"
  }
}

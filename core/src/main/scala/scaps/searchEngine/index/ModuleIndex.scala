package scaps.searchEngine.index

import upickle._
import org.apache.lucene.store.Directory
import scaps.webapi.Module
import org.apache.lucene.document.Document
import org.apache.lucene.document.TextField
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.StoredField
import org.apache.lucene.analysis.core.KeywordAnalyzer
import scala.util.Try
import org.apache.lucene.search.MatchAllDocsQuery
import org.apache.lucene.index.Term

class ModuleIndex(val dir: Directory) extends Index[Module] {
  import ModuleIndex._

  val analyzer = new KeywordAnalyzer

  def addEntities(entities: Seq[Module]): Try[Unit] =
    withWriter { writer =>
      entities.foreach { entity =>
        val doc = toDocument(entity)
        writer.updateDocument(new Term(fields.moduleId, entity.moduleId), doc)
      }
    }

  def allModules(): Try[Seq[Module]] =
    search(new MatchAllDocsQuery)

  def deleteModule(module: Module): Try[Unit] =
    withWriter { writer =>
      writer.deleteDocuments(new Term(fields.moduleId, module.moduleId))
    }

  override def toDocument(entity: Module): Document = {
    val doc = new Document

    def add(field: String, value: String) =
      doc.add(new TextField(field, value, Store.YES))

    add(fields.moduleId, entity.moduleId)

    doc.add(new StoredField(fields.entity, upickle.write(entity)))

    doc
  }

  override def toEntity(doc: Document): Module = {
    val json = doc.getValues(fields.entity)(0)

    upickle.read[Module](json)
  }
}

object ModuleIndex {
  object fields {
    val moduleId = "moduleId"
    val entity = "entity"
  }
}

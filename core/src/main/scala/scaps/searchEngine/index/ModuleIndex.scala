/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine.index

import upickle._
import org.apache.lucene.store.Directory
import scaps.api.Module
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

  def deleteModule(module: Module): Try[Unit] =
    withWriter { writer =>
      writer.deleteDocuments(new Term(fields.moduleId, module.moduleId))
    }

  def toDocument(entity: Module): Document = {
    val doc = new Document

    doc.add(new TextField(fields.moduleId, entity.moduleId, Store.YES))
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

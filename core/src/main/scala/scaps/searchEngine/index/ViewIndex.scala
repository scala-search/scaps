package scaps.searchEngine.index

import scala.util.Try
import scala.collection.JavaConverters._
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.TermQuery
import org.apache.lucene.store.Directory
import ViewIndex.fields
import scaps.api.Contravariant
import scaps.api.Covariant
import scaps.api.Invariant
import scaps.api.TypeRef
import scaps.api.ViewDef
import scaps.api.Module

class ViewIndex(val dir: Directory) extends Index[ViewDef] {
  import ViewIndex._

  val analyzer = new KeywordAnalyzer

  def addEntities(entities: Seq[ViewDef]): Try[Unit] =
    withWriter { writer =>
      entities.foreach { e =>
        val idTerm = new Term(fields.id, id(e))
        val doc = toDocument(e)
        writer.updateDocument(idTerm, doc)
      }
    }

  def findAlternativesWithDistance(tpe: TypeRef, moduleIds: Set[String] = Set()): Try[Seq[(TypeRef, Float)]] = Try {
    findViews(tpe, moduleIds).get
      .flatMap { view =>
        view(tpe).map((_, view.distance))
      }
      .distinct
  }

  private def findViews(tpe: TypeRef, moduleIds: Set[String]): Try[Seq[ViewDef]] =
    Try {
      val altsOfGenericTpe =
        if (tpe.args.exists(!_.isTypeParam))
          findViews(tpe.withArgsAsParams, moduleIds).get
        else if (!tpe.isTypeParam)
          findViews(tpe.copy(args = Nil, isTypeParam = true), moduleIds).get
        else
          Seq()

      val query = new BooleanQuery()
      query.add(new TermQuery(new Term(fields.from, ViewDef.key(tpe))), Occur.MUST);
      query.add(Index.moduleQuery(moduleIds, fields.moduleId), Occur.MUST)

      altsOfGenericTpe ++ search(query).get
    }

  def deleteEntitiesIn(module: Module): Try[Unit] =
    withWriter { writer =>
      writer.deleteDocuments(new Term(fields.moduleId, module.moduleId))
    }

  def toDocument(v: ViewDef) = {
    val doc = new Document

    doc.add(new TextField(fields.id, id(v), Store.YES))
    doc.add(new TextField(fields.name, v.name, Store.YES))
    doc.add(new TextField(fields.from, v.fromKey, Store.YES))
    doc.add(new TextField(fields.moduleId, v.module.moduleId, Store.YES))
    doc.add(new StoredField(fields.entity, upickle.write(v)))
    doc
  }

  private def id(v: ViewDef) =
    s"${v.module.moduleId}:${v.name}"

  override def toEntity(doc: Document): ViewDef = {
    val json = doc.getValues(fields.entity)(0)

    upickle.read[ViewDef](json)
  }
}

object ViewIndex {
  object fields {
    val id = "id"
    val name = "name"
    val from = "from"
    val entity = "entity"
    val moduleId = "moduleId"
  }
}

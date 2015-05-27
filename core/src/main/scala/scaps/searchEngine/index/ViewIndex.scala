package scaps.searchEngine.index

import scaps.searchEngine.View
import scaps.searchEngine.ImplicitConversion
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.store.Directory
import org.apache.lucene.document.Document
import org.apache.lucene.document.TextField
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.StoredField
import upickle._
import scaps.webapi._
import org.apache.lucene.search.TermQuery
import org.apache.lucene.index.Term
import scala.util.Try

class ViewIndex(val dir: Directory) extends Index[View] {
  import ViewIndex._

  val analyzer = new KeywordAnalyzer

  def addEntities(entities: Seq[View]): Try[Unit] =
    withWriter { writer =>
      entities.foreach { entity =>
        val doc = toDocument(entity)
        writer.updateDocument(new Term(fields.id, viewId(entity)), doc)
      }
    }

  def findViews(tpe: TypeEntity): Try[Seq[View]] = Try {
    tpe.variance match {
      case Covariant =>
        findViewsTo(tpe).get
      case Contravariant =>
        findViewsFrom(tpe).get
      case Invariant if tpe.name != TypeEntity.Unknown.name =>
        List(ImplicitConversion(tpe, TypeEntity.Unknown(Invariant), ""))
      case Invariant =>
        Nil
    }
  }

  def findViewsFrom(tpe: TypeEntity) = {
    val q = new TermQuery(new Term(fields.from, View.key(tpe)))

    search(q)
  }

  def findViewsTo(tpe: TypeEntity) = {
    val q = new TermQuery(new Term(fields.to, View.key(tpe)))

    search(q)
  }

  private def viewId(v: View) =
    s"{${v.fromKey}}->{${v.toKey}}"

  override def toDocument(v: View) = {
    val doc = new Document

    doc.add(new TextField(fields.id, viewId(v), Store.YES))
    doc.add(new TextField(fields.from, v.fromKey, Store.YES))
    doc.add(new TextField(fields.to, v.toKey, Store.YES))

    doc.add(new StoredField(fields.entity, upickle.write(v)))

    doc
  }

  override def toEntity(doc: Document): View = {
    val json = doc.getValues(fields.entity)(0)

    upickle.read[View](json)
  }
}

object ViewIndex {
  object fields {
    val id = "id"
    val from = "from"
    val to = "to"
    val entity = "entity"
  }
}

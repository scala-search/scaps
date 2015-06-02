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

  def findAlternativesWithDistance(tpe: TypeEntity): Try[Seq[(TypeEntity, Int)]] = Try {
    def alignVariance(alt: TypeEntity) =
      alt.withVariance(tpe.variance)

    def alignTypeArgs(alt: TypeEntity, source: TypeEntity): TypeEntity = {
      val alignedArgs = alt.args.map { arg =>
        source.args.indexOf(arg) match {
          case -1     => alignTypeArgs(arg, source)
          case argIdx => alignTypeArgs(tpe.args(argIdx), source)
        }
      }

      alt.copy(args = alignedArgs)
    }

    val (sources, alternatives, distances) = (tpe.variance match {
      case Covariant =>
        findViewsTo(tpe).get.map(view => (view.to, view.from, view.distance))
      case Contravariant =>
        findViewsFrom(tpe).get.map(view => (view.from, view.to, view.distance))
      case Invariant if tpe.name != TypeEntity.Unknown.name =>
        List((tpe, TypeEntity.Unknown(Invariant), 1))
      case Invariant =>
        Nil
    }).unzip3

    alternatives
      .map(alignVariance)
      .zip(sources.map(alignVariance))
      .map((alignTypeArgs _).tupled)
      .zip(distances)
  }

  private def findViewsFrom(tpe: TypeEntity): Try[Seq[View]] =
    findViews(tpe, fields.from)

  private def findViewsTo(tpe: TypeEntity): Try[Seq[View]] =
    findViews(tpe, fields.to)

  private def findViews(tpe: TypeEntity, field: String): Try[Seq[View]] =
    Try {
      val altsOfGenericTpe =
        if (tpe.args.exists(!_.isTypeParam))
          findViews(tpe.withArgsAsParams, field).get
        else
          Seq()

      altsOfGenericTpe ++ search(new TermQuery(new Term(field, View.key(tpe)))).get
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

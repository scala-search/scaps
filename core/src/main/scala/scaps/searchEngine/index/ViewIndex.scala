package scaps.searchEngine.index

import scala.util.Try

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.Term
import org.apache.lucene.search.TermQuery
import org.apache.lucene.store.Directory

import scaps.api.Contravariant
import scaps.api.Covariant
import scaps.api.Invariant
import scaps.api.TypeRef
import scaps.api.View

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

  def findAlternativesWithDistance(tpe: TypeRef): Try[Seq[(TypeRef, Float)]] = Try {
    def alignVariance(alt: TypeRef) =
      alt.withVariance(tpe.variance)

    def alignTypeArgs(source: TypeRef, alt: TypeRef): TypeRef = {
      val alignedArgs = alt.args.map { arg =>
        source.args.indexWhere(_.name == arg.name) match {
          case -1     => alignTypeArgs(source, arg)
          case argIdx => alignTypeArgs(source, tpe.args(argIdx).withVariance(arg.variance))
        }
      }

      alt.copy(args = alignedArgs)
    }

    val (sources, alternatives, distances) = (tpe.variance match {
      case Covariant if tpe.name == TypeRef.Nothing.name =>
        Nil
      case Covariant =>
        (tpe, TypeRef.Nothing(Covariant), 1f) +:
          findViewsTo(tpe).get.map(view => (view.to, view.from, view.distance))
      case Contravariant =>
        findViewsFrom(tpe).get.map(view => (view.from, view.to, view.distance))
      case Invariant if tpe.name == TypeRef.Unknown.name =>
        Nil
      case Invariant =>
        List((tpe, TypeRef.Unknown(Invariant), 1f))
    }).unzip3

    sources.map(alignVariance)
      .zip(alternatives.map(alignVariance))
      .map((alignTypeArgs _).tupled)
      .zip(distances)
  }

  private def findViewsFrom(tpe: TypeRef): Try[Seq[View]] =
    findViews(tpe, fields.from)

  private def findViewsTo(tpe: TypeRef): Try[Seq[View]] =
    findViews(tpe, fields.to)

  private def findViews(tpe: TypeRef, field: String): Try[Seq[View]] =
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

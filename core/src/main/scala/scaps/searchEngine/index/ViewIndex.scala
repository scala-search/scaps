package scaps.searchEngine.index

import scala.util.Try
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

  def addEntities(entities: Seq[ViewDef]): Try[Unit] =Try {
    val distinctEntities = entities.distinct
    val indexedViews = allEntities().get

    val entitiesWithModules = distinctEntities.map { view =>
      indexedViews.find(_.name == view.name)
        .fold(view) { indexedView =>
          view.copy(modules = view.modules ++ indexedView.modules)
        }
    }

    withWriter { writer =>
      entitiesWithModules.foreach { entity =>
        val doc = toDocument(entity)
        writer.updateDocument(new Term(fields.name, entity.name), doc)
      }
    }.get
  }

  def findAlternativesWithDistance(tpe: TypeRef, moduleIds: Set[String] = Set()): Try[Seq[(TypeRef, Float)]] = Try {
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
          findViewsTo(tpe, moduleIds).get.map(view => (view.to, view.from, view.distance))
      case Contravariant =>
        findViewsFrom(tpe, moduleIds).get.map(view => (view.from, view.to, view.distance))
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

  private def findViewsFrom(tpe: TypeRef, moduleIds: Set[String]): Try[Seq[ViewDef]] =
    findViews(tpe, fields.from, moduleIds)

  private def findViewsTo(tpe: TypeRef, moduleIds: Set[String]): Try[Seq[ViewDef]] =
    findViews(tpe, fields.to, moduleIds)

  private def findViews(tpe: TypeRef, field: String, moduleIds: Set[String]): Try[Seq[ViewDef]] =
    Try {
      val altsOfGenericTpe =
        if (tpe.args.exists(!_.isTypeParam))
          findViews(tpe.withArgsAsParams, field, moduleIds).get
        else
          Seq()

      val query = new BooleanQuery()
      query.add(new TermQuery(new Term(field, ViewDef.key(tpe))), Occur.MUST);
      query.add(Index.moduleQuery(moduleIds, fields.modules), Occur.MUST)

      altsOfGenericTpe ++ search(query).get
    }

  def deleteEntitiesIn(module: Module): Try[Unit] = Try {
    val q = new TermQuery(new Term(fields.modules, module.moduleId))
    val viewDefsInModule = search(q).get

    withWriter { writer =>
      viewDefsInModule.foreach { v =>
        val vTerm = new Term(fields.name, v.name)

        val vWithoutModule = v.copy(modules = v.modules - module)

        if (vWithoutModule.modules.isEmpty) {
          writer.deleteDocuments(vTerm)
        } else {
          writer.updateDocument(vTerm, toDocument(vWithoutModule))
        }
      }
    }.get
  }

  override def toDocument(v: ViewDef) = {
    val doc = new Document

    doc.add(new TextField(fields.name, v.name, Store.YES))
    doc.add(new TextField(fields.from, v.fromKey, Store.YES))
    doc.add(new TextField(fields.to, v.toKey, Store.YES))

    v.modules.foreach { m =>
      doc.add(new TextField(fields.modules, m.moduleId, Store.YES))
    }

    doc.add(new StoredField(fields.entity, upickle.write(v)))

    doc
  }

  override def toEntity(doc: Document): ViewDef = {
    val json = doc.getValues(fields.entity)(0)

    upickle.read[ViewDef](json)
  }
}

object ViewIndex {
  object fields {
    val name = "name"
    val from = "from"
    val to = "to"
    val entity = "entity"
    val modules = "modules"
  }
}

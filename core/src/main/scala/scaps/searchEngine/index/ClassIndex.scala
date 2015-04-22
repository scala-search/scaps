package scaps.searchEngine.index

import scaps.model.ClassEntity
import scaps.model.TypeEntity
import scaps.settings.Settings
import scala.util.Try

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.MatchAllDocsQuery
import org.apache.lucene.search.TermQuery
import org.apache.lucene.store.Directory

/**
 * Persists class entities and provides lookup for classes by name.
 *
 * This index is mainly used for fast access to class hierarchies for query building.
 */
class ClassIndex(val dir: Directory, settings: Settings) extends Index[ClassEntity] {
  import ClassIndex._

  val analyzer = new KeywordAnalyzer

  override def addEntities(entities: Seq[ClassEntity]): Try[Unit] =
    withWriter { writer =>
      entities.foreach { entity =>
        val doc = toDocument(entity)
        writer.updateDocument(new Term(fields.name, entity.name), doc)
      }
    }

  /**
   * Searches for class entities whose last parts of the full qualified name are `suffix`
   * and accept `noArgs` type parameters.
   */
  def findClassBySuffix(suffix: String): Try[Seq[ClassEntity]] = {
    val query = new BooleanQuery()
    query.add(new TermQuery(new Term(fields.suffix, suffix)), Occur.MUST)

    search(query)
  }

  def findSubClasses(tpe: TypeEntity): Try[Seq[ClassEntity]] = {
    def partialTypes(tpe: TypeEntity): List[TypeEntity] = {
      def argPerms(args: List[TypeEntity]): List[List[TypeEntity]] = args match {
        case Nil => List(Nil)
        case a :: as =>
          for {
            aPerm <- TypeEntity("_") :: partialTypes(a)
            asPerm <- argPerms(as)
          } yield aPerm :: asPerm
      }

      argPerms(tpe.args).map(perm => tpe.copy(args = perm))
    }

    val q = new BooleanQuery
    for (perm <- partialTypes(tpe)) {
      q.add(new TermQuery(new Term(fields.baseClass, perm.signature)), Occur.SHOULD)
    }

    search(q)
  }

  def allClasses(): Try[Seq[ClassEntity]] =
    search(new MatchAllDocsQuery)

  override def toDocument(entity: ClassEntity): Document = {
    val doc = new Document

    doc.add(new TextField(fields.name, entity.name, Field.Store.YES))
    for (suffix <- suffixes(entity.name)) {
      doc.add(new TextField(fields.suffix, suffix, Field.Store.NO))
    }
    for (baseClass <- entity.baseTypes) {
      val withWildcards = baseClass.renameTypeParams(entity.typeParameters, _ => "_")
      doc.add(new TextField(fields.baseClass, withWildcards.signature, Field.Store.YES))
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

  override def toEntity(doc: Document): ClassEntity = {
    val bytes = doc.getBinaryValues(fields.entity).flatMap(_.bytes)

    Serialization.unpickleClass(bytes)
  }
}

object ClassIndex {
  object fields {
    val name = "name"
    val suffix = "suffix"
    val baseClass = "baseClass"
    val entity = "entity"
  }
}

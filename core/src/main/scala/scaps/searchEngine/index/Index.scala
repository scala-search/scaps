package scaps.searchEngine.index

import scala.collection.JavaConverters._
import scaps.utils.using
import scala.util.Try
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.document.Document
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.Query
import org.apache.lucene.search.similarities.DefaultSimilarity
import org.apache.lucene.search.similarities.Similarity
import org.apache.lucene.store.Directory
import org.apache.lucene.util.Version
import org.apache.lucene.search.DocIdSetIterator
import org.apache.lucene.search.Explanation

trait Index[E] {
  private[index] def dir: Directory
  private[index] def analyzer: Analyzer

  private[index] val similarity: Similarity = new DefaultSimilarity

  def resetIndex(): Try[Unit] = Try {
    dir.listAll().foreach(dir.deleteFile(_))
    writerInitialized = false
  }

  private[index] def toDocument(e: E): Document
  private[index] def toEntity(d: Document): E

  private[index] def search(query: Query, maxResults: Int = Int.MaxValue, explain: Option[(E, Explanation) => Unit] = None): Try[Seq[E]] =
    withSearcher { searcher =>
      val docs = searcher.search(query, maxResults)

      for {
        i <- 0 until docs.scoreDocs.length
        docId = docs.scoreDocs(i).doc
        if docId != DocIdSetIterator.NO_MORE_DOCS
      } yield {
        val res = toEntity(searcher.doc(docId))
        explain.foreach { e =>
          e(res, searcher.explain(query, docId))
        }
        res
      }
    }

  private[index] def withWriter[A](f: IndexWriter => A): Try[A] = {
    val writerConf = new IndexWriterConfig(Version.LUCENE_4_10_4, analyzer)
    writerConf.setSimilarity(similarity)

    using(new IndexWriter(dir, writerConf)) { w =>
      f(w)
    }
  }

  @volatile
  private var writerInitialized = false

  private[index] def withSearcher[A](f: IndexSearcher => A): Try[A] = {
    if (!writerInitialized) {
      initWriter()
      writerInitialized = true
    }

    using(DirectoryReader.open(dir)) { reader =>
      val searcher = new IndexSearcher(reader)
      searcher.setSimilarity(similarity)
      f(searcher)
    }
  }

  def initWriter() = {
    withWriter(_ => ()).get
  }

  def replaceAllEntities(entities: Seq[E]): Try[Unit] =
    withWriter { writer =>
      val docs = entities.map(toDocument)
      writer.deleteAll()
      writer.addDocuments(docs.asJavaCollection)
    }
}

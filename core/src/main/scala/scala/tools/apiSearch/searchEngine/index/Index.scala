package scala.tools.apiSearch.searchEngine.index

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.tools.apiSearch.utils.using
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

trait Index[E] {
  private[index] def dir: Directory
  private[index] def analyzer: Analyzer

  private[index] val similarity: Similarity = new DefaultSimilarity

  def delete(): Try[Unit] = Try {
    dir.listAll().foreach(dir.deleteFile(_))
  }

  private[index] def toDocument(e: E): Document
  private[index] def toEntity(d: Document): E

  def addEntities(entities: Seq[E]): Try[Unit] =
    withWriter { writer =>
      val docs = entities.map(toDocument)
      Try(writer.addDocuments(docs.asJava))
    }

  private[index] def search(query: Query, maxResults: Int = Int.MaxValue): Try[Seq[E]] =
    withSearcher { searcher =>
      val docs = searcher.search(query, maxResults)

      docs.scoreDocs.map(scoreDoc =>
        toEntity(searcher.doc(scoreDoc.doc)))
    }

  private def withWriter[A](f: IndexWriter => A): Try[A] = {
    val writerConf = new IndexWriterConfig(Version.LUCENE_4_10_4, analyzer)
    writerConf.setSimilarity(similarity)

    using(new IndexWriter(dir, writerConf)) { w =>
      f(w)
    }
  }

  private def withSearcher[A](f: IndexSearcher => A): Try[A] = {
    using(DirectoryReader.open(dir)) { reader =>
      val searcher = new IndexSearcher(reader)
      searcher.setSimilarity(similarity)
      f(searcher)
    }
  }
}

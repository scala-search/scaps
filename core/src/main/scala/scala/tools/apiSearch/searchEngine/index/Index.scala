package scala.tools.apiSearch.searchEngine.index

import scala.tools.apiSearch.utils.using
import scala.util.Try

import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.similarities.DefaultSimilarity
import org.apache.lucene.search.similarities.Similarity
import org.apache.lucene.store.Directory
import org.apache.lucene.util.Version

trait Index {
  def dir: Directory
  def analyzer: Analyzer

  val similarity: Similarity = new DefaultSimilarity

  def delete(): Try[Unit] = Try {
    dir.listAll().foreach(dir.deleteFile(_))
  }

  def withWriter[A](f: IndexWriter => A): Try[A] = {
    val writerConf = new IndexWriterConfig(Version.LUCENE_4_10_4, analyzer)
    writerConf.setSimilarity(similarity)

    using(new IndexWriter(dir, writerConf)) { w =>
      f(w)
    }
  }

  def withSearcher[A](f: IndexSearcher => A): Try[A] = {
    using(DirectoryReader.open(dir)) { reader =>
      val searcher = new IndexSearcher(reader)
      searcher.setSimilarity(similarity)
      f(searcher)
    }
  }
}
package scala.tools.apiSearch.index

import org.apache.lucene.store.Directory
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.index.IndexWriter
import scala.util.Try
import org.apache.lucene.index.IndexWriterConfig
import scala.tools.apiSearch.utils.using
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.util.Version

trait Index {
  def dir: Directory
  def analyzer: Analyzer

  val maxResults = 1000

  def delete(): Try[Unit] = Try {
    dir.listAll().foreach(dir.deleteFile(_))
  }

  def withWriter[A](f: IndexWriter => A): Try[A] = {
    val writerConf = new IndexWriterConfig(Version.LUCENE_4_10_4, analyzer)

    using(new IndexWriter(dir, writerConf)) { w =>
      Try(f(w))
    }
  }

  def withSearcher[A](f: IndexSearcher => A): Try[A] = {
    using(DirectoryReader.open(dir)) { reader =>
      Try(f(new IndexSearcher(reader)))
    }
  }
}

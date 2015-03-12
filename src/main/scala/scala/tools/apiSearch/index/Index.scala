package scala.tools.apiSearch.index

import org.apache.lucene.store.Directory
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.index.IndexWriter
import scala.util.Try
import org.apache.lucene.index.IndexWriterConfig
import scala.tools.apiSearch.utils.using
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.index.DirectoryReader

trait Index {
  def dir: Directory
  def analyzer: Analyzer

  def withWriter[A](f: IndexWriter => A): Try[A] = {
    val writerConf = new IndexWriterConfig(analyzer)

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

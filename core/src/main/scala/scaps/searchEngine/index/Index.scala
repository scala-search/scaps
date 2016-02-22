/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

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
import org.apache.lucene.search.MatchAllDocsQuery
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.TermQuery
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import scaps.api.Module
import scaps.api.Result
import scalaz.std.boolean

trait Index[E] {
  private[index] def dir: Directory
  private[index] def analyzer: Analyzer

  private[index] val similarity: Similarity = new DefaultSimilarity

  def resetIndex(): Try[Unit] = Try {
    dir.listAll().foreach(dir.deleteFile(_))
    writerInitialized = false
  }

  def allEntities(): Try[Seq[E]] = Try {
    search(new MatchAllDocsQuery).get.map(_.entity)
  }

  private[index] def toEntity(d: Document): E

  private[index] def search(query: Query, maxResults: Int = Int.MaxValue, explain: Boolean = false): Try[Seq[Result[E]]] =
    withSearcher { searcher =>
      val docs = searcher.search(query, maxResults)

      for {
        i <- 0 until docs.scoreDocs.length
        docId = docs.scoreDocs(i).doc
        if docId != DocIdSetIterator.NO_MORE_DOCS
      } yield {
        val res = toEntity(searcher.doc(docId))
        Result(res, docs.scoreDocs(i).score,
          boolean.option(explain, searcher.explain(query, docId).toString()))
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

  private def initWriter() = {
    withWriter(_ => ()).get
  }
}

object Index {
  def moduleQuery(selectedModuleIds: Set[String], moduleField: String) = {
    val q = new BooleanQuery

    if (!selectedModuleIds.isEmpty) {
      selectedModuleIds.foreach { moduleId =>
        val tq = new TermQuery(new Term(moduleField, moduleId))
        q.add(tq, Occur.SHOULD)
      }
    } else {
      q.add(new MatchAllDocsQuery, Occur.SHOULD)
    }
    q.setBoost(0)

    q
  }
}

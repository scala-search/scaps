package scaps.searchEngine.index

import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.TextField
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.Term
import org.apache.lucene.search.ConstantScoreQuery
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.TermQuery
import org.apache.lucene.search.similarities.DefaultSimilarity
import org.apache.lucene.store.RAMDirectory
import org.apache.lucene.util.Version
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scaps.utils.using

class MaxScoreQuerySpecs extends FlatSpec with Matchers {
  // This spec does not use the lucene test framework because it does not play well
  // with SBTs dependency resolution. It would be doable but is probably not worth the pain.

  val docs = List("a", "a b", "a b c").map { content =>
    val doc = new Document
    doc.add(new TextField("content", content, Field.Store.YES))
    doc
  }

  "a max score query" should "behave identical to its single sub query" in
    withSearcher(docs) { searcher =>
      val tq = new TermQuery(new Term("content", "b"))
      val msq = MaxScoreQuery(tq)

      val tqRes = searcher.search(tq, 10).scoreDocs.toSeq
      val msqRes = searcher.search(msq, 10).scoreDocs.toSeq

      tqRes.toString should be(msqRes.toString)
    }

  it should "not fail when subqueries match no documents" in
    withSearcher(docs) { searcher =>
      val msq = MaxScoreQuery(
        new TermQuery(new Term("content", "x")),
        new TermQuery(new Term("content", "y")))

      val msqRes = searcher.search(msq, 10).scoreDocs.toSeq

      msqRes.length should be(0)
    }

  it should "only score with the best score of its sub queries" in
    withSearcher(docs) { searcher =>
      val tqa = new TermQuery(new Term("content", "a"))
      tqa.setBoost(5) // ensures that "a" always scores higher than "b"
      val tqb = new TermQuery(new Term("content", "b"))
      val msq = MaxScoreQuery(tqa, tqb)

      val tqaRes = searcher.search(tqa, 10).scoreDocs.toSeq
      val msqRes = searcher.search(msq, 10).scoreDocs.toSeq

      // tqa and msq yield same docs with same score
      tqaRes.toString should be(msqRes.toString)
    }

  it should "only score with the best score of its sub queries 2" in
    withSearcher(docs) { searcher =>
      val tqa = new ConstantScoreQuery(new TermQuery(new Term("content", "a")))
      tqa.setBoost(1)
      val tqb = new ConstantScoreQuery(new TermQuery(new Term("content", "b")))
      tqb.setBoost(2)
      val msq = MaxScoreQuery(tqa, tqb)

      val msqRes = searcher.search(msq, 10).scoreDocs.toSeq

      val firstDoc = msqRes.find(_.doc == 0).get
      val secondDoc = msqRes.find(_.doc == 1).get
      val thirdDoc = msqRes.find(_.doc == 2).get

      // we can only make relative comparisons due to normalization
      firstDoc.score should be > (0f)
      firstDoc.score should be < (secondDoc.score)
      secondDoc.score should be(thirdDoc.score)
    }

  it should "not fail when one subquery matches no documents" in
    withSearcher(docs) { searcher =>
      val msq = MaxScoreQuery(
        new TermQuery(new Term("content", "a")),
        new TermQuery(new Term("content", "x")))

      val msqRes = searcher.search(msq, 10).scoreDocs.toSeq

      msqRes.length should be(3)
    }

  it should "support boosts" in
    withSearcher(docs) { searcher =>
      val tq = new TermQuery(new Term("content", "a"))
      val msq = MaxScoreQuery(tq)
      val msqBoosted = MaxScoreQuery(tq)
      msqBoosted.setBoost(5)

      val msqRes = searcher.search(msq, 10).scoreDocs.toSeq
      val msqBoostedRes = searcher.search(msqBoosted, 10).scoreDocs.toSeq

      msqRes.zip(msqBoostedRes).foreach {
        case (r, boostedR) =>
          r.doc should be(boostedR.doc)
          (r.score * 5) should be(boostedR.score)
      }
    }

  it should "create an explanation when matching" in
    withSearcher(docs) { searcher =>
      val msq = MaxScoreQuery(
        new TermQuery(new Term("content", "a")),
        new TermQuery(new Term("content", "b")),
        new TermQuery(new Term("content", "x")))

      val expl = searcher.explain(msq, 1) // explain for doc "a b"

      expl.getDescription should include("max")
      expl.isMatch should be(true)

      // term "b" contributes higher score than "a"
      val explB = expl.getDetails.find(_.getDescription.contains("content:b")).get

      expl.getValue should be(explB.getValue)
    }

  it should "create an explanation when not matching" in
    withSearcher(docs) { searcher =>
      val msq = MaxScoreQuery(
        new TermQuery(new Term("content", "x")),
        new TermQuery(new Term("content", "y")))

      val expl = searcher.explain(msq, 1) // explain "a b"

      expl.getDescription should include("max")
      expl.isMatch should be(false)
      expl.getValue should be(0f)
    }

  val analyzer = new WhitespaceAnalyzer
  val similarity = new DefaultSimilarity

  def withSearcher(docs: Seq[Document])(f: IndexSearcher => Unit) = {
    using(new RAMDirectory) { dir =>
      val writerConf = new IndexWriterConfig(Version.LUCENE_4_10_4, analyzer)
      writerConf.setSimilarity(similarity)

      using(new IndexWriter(dir, writerConf)) { indexWriter =>
        docs.foreach { doc =>
          indexWriter.addDocument(doc)
        }
        indexWriter.commit()

        using(DirectoryReader.open(dir)) { reader =>
          val searcher = new IndexSearcher(reader)
          searcher.setSimilarity(similarity)
          f(searcher)
        }.get
      }.get
    }.get
  }
}

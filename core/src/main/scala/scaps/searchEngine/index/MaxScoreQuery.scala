package scaps.searchEngine.index

import org.apache.lucene.index.AtomicReaderContext
import org.apache.lucene.search.ComplexExplanation
import org.apache.lucene.search.DocIdSetIterator
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.Query
import org.apache.lucene.search.Scorer
import org.apache.lucene.search.Weight
import org.apache.lucene.util.Bits

/**
 * A query that does only count the maximum score of its sub queries.
 *
 * This matches the same documents as a boolean OR query but does not accumulate
 * the scores of the matching sub queries.
 */
case class MaxScoreQuery(subQueriesHead: Query, subQueriesTail: Query*) extends Query {
  query =>

  val subQueries = subQueriesHead +: subQueriesTail

  override def createWeight(searcher: IndexSearcher): Weight = {
    val subWeights = subQueries.map(_.createWeight(searcher))

    new Weight {
      weight =>

      override def explain(context: AtomicReaderContext, doc: Int) = {
        val s = scorer(context, context.reader().getLiveDocs())

        val expl = new ComplexExplanation
        expl.setDescription("max of:")

        if (s != null && s.advance(doc) == doc) {
          expl.setMatch(true)
          expl.setValue(s.score())

          subWeights.foreach { w =>
            val subExpl = w.explain(context, doc)
            expl.addDetail(subExpl)
          }
        } else {
          expl.setMatch(false)
        }

        expl
      }

      override def getQuery() =
        query

      override def getValueForNormalization() =
        // not sure if max is a feasible implementation but it seems to give reasonable results
        subWeights.map(_.getValueForNormalization).max

      override def normalize(norm: Float, topLevelBoost: Float) = {
        val incorporatedBoost = query.getBoost * topLevelBoost
        subWeights.foreach(_.normalize(norm, incorporatedBoost))
      }

      override def scorer(context: AtomicReaderContext, acceptDocs: Bits) =
        new Scorer(weight) {
          val subScorers = subWeights.flatMap { w =>
            Option(w.scorer(context, acceptDocs))
          }

          var currentDocId = -1

          updateCurrentDocId()

          override def score() =
            scorersAtCurrentPos.map(_.score()).max

          override def freq() =
            scorerWithMaxScore.freq()

          override def docID() =
            currentDocId

          override def nextDoc() = {
            scorersAtCurrentPos.foreach(_.nextDoc())
            updateCurrentDocId()
          }

          override def advance(target: Int) = {
            subScorers.foreach(_.advance(target))
            updateCurrentDocId()
          }

          override def cost() =
            subScorers.map(_.cost()).max

          def updateCurrentDocId() = {
            currentDocId = subScorers
              .map(_.docID())
              .reduceOption(math.min _) // subScorers may be empty
              .getOrElse(DocIdSetIterator.NO_MORE_DOCS)
            currentDocId
          }

          def scorersAtCurrentPos = subScorers.filter(_.docID() == currentDocId)

          def scorerWithMaxScore =
            scorersAtCurrentPos
              .map(s => (s, s.score()))
              .maxBy(_._2)._1
        }
    }
  }

  override def toString(field: String) = {
    s"MaxScore(${subQueries.map(_.toString(field)).mkString(", ")})"
  }
}

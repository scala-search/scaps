package scaps.searchEngine.index

import org.apache.lucene.index.AtomicReaderContext
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.Term
import org.apache.lucene.queries.CustomScoreProvider
import org.apache.lucene.queries.CustomScoreQuery
import org.apache.lucene.queries.function.FunctionQuery
import org.apache.lucene.queries.function.valuesource.NormValueSource
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.ConstantScoreQuery
import org.apache.lucene.search.Explanation
import org.apache.lucene.search.Query
import org.apache.lucene.search.TermQuery
import scaps.searchEngine.ApiTypeQuery
import scaps.utils.Logging
import scaps.api.Contravariant
import scaps.api.Covariant
import scaps.settings.QuerySettings

/**
 * A Lucene query that scores type fingerprints in a field against a type query.
 */
class TypeFingerprintQuery(field: String, apiQuery: ApiTypeQuery, subQuery: Query, settings: QuerySettings)
    extends CustomScoreQuery(
      TypeFingerprintQuery.matcherQuery(field, apiQuery, subQuery, settings.fingerprintFrequencyCutoff)) {

  val scorer = TypeFingerprintQuery.FingerprintScorer(apiQuery)

  override def name() = "TypeFingerprint"

  override def rewrite(reader: IndexReader) = this

  override def getCustomScoreProvider(context: AtomicReaderContext) =
    new CustomScoreProvider(context) {
      val reader = context.reader()

      override def customScore(doc: Int, subQueryScore: Float, valSrcScores: Array[Float]): Float =
        (score(doc)._1) + normDocScore(subQueryScore)

      def normDocScore(docScore: Float): Float =
        (settings.docBoost * math.log(docScore + 1)).toFloat

      def score(doc: Int) = {
        // There is probably a faster way to access the field value for every matched document.
        // Though, accessing the fingerprint through value vectors resulted in slightly worse performance.
        val fingerprint = reader.document(doc).getValues(field)

        val (fpScore, penalty, exp) = scorer.score(fingerprint)
        val penalized = (1d / (math.pow(settings.penaltyWeight * penalty, 2) + 1)).toFloat * fpScore

        (penalized, penalty, exp)
      }

      override def customExplain(doc: Int, subQueryExpl: Explanation, valSrcExpls: Array[Explanation]): Explanation = {
        val (queryScore, penalty, scoresPerTerm) = score(doc)

        val docScore = normDocScore(subQueryExpl.getValue)
        val expl = new Explanation(
          (queryScore) + docScore,
          s"type fingerprint score, typeFingerprint + $docScore of:")
        expl.addDetail(new Explanation(queryScore, s"type fingerprint ${scoresPerTerm.mkString(", ")}, penalty: $penalty"))
        expl.addDetail(subQueryExpl)
        expl
      }
    }
}

object TypeFingerprintQuery extends Logging {
  /**
   * Creates a matcher query that matches all documents containing at least one fingerprint type
   * of `typeQuery` that is more relevant than one of the threshold types. Additionally, it also
   * matches every document that is matched by `subQuery`.
   *
   * The matcher query scores fingerprints with a constant value of 1 or uses the score of `subQuery`
   * if available.
   */
  def matcherQuery(field: String, typeQuery: ApiTypeQuery, subQuery: Query, frequencyCutoff: Double) = {
    val fingerprintMatcher = new BooleanQuery

    val values = valuesBelowCutoff(typeQuery, frequencyCutoff)

    logger.debug(s"Matching documents with fingerprint types: $values")

    for {
      value <- values
    } {
      fingerprintMatcher.add(new TermQuery(new Term(field, value)), Occur.SHOULD)
    }

    val matcherQuery = new BooleanQuery
    val constantMatcher = new ConstantScoreQuery(fingerprintMatcher)
    constantMatcher.setBoost(0f)
    matcherQuery.add(constantMatcher, Occur.SHOULD)
    matcherQuery.add(subQuery, Occur.SHOULD)

    matcherQuery
  }

  def valuesBelowCutoff(typeQuery: ApiTypeQuery, frequencyCutoff: Double): List[String] = {
    val rankedTermsWithFreq = typeQuery.allTypes
      .sortBy(-_.boost)
      .map(t => (t.fingerprint, t.typeFrequency))
      .distinct

    val (values, _) = rankedTermsWithFreq.foldLeft((List[String](), 0f)) {
      case (acc @ (accTerms, accFreq), (value, freq)) =>
        if (accFreq + freq < frequencyCutoff)
          (value :: accTerms, accFreq + freq)
        else
          acc
    }

    values
  }

  object FingerprintScorer {
    def apply(q: ApiTypeQuery): FingerprintScorer = {
      def rec(q: ApiTypeQuery): FingerprintScorer =
        q match {
          case ApiTypeQuery.Sum(children) =>
            SumNode(children.map(apply))
          case ApiTypeQuery.Max(children) =>
            MaxNode(children.map(apply))
          case t @ ApiTypeQuery.Type(v, name, boost, _) =>
            Leaf(t.fingerprint, boost.toFloat)
        }

      minimize(rec(q))
    }

    def minimize(scorer: FingerprintScorer): FingerprintScorer = scorer match {
      case DeadLeaf | (_: Leaf)        => scorer
      case SumNode(Nil) | MaxNode(Nil) => DeadLeaf
      case SumNode(child :: Nil)       => minimize(child)
      case MaxNode(child :: Nil)       => minimize(child)
      case SumNode(cs)                 => SumNode(cs.map(minimize))
      case MaxNode(cs)                 => MaxNode(cs.map(minimize))
    }
  }

  sealed trait FingerprintScorer {

    def score(fingerprint: Seq[String]): (Float, Float, List[(String, Float)]) = {
      /*
       * This is just an heuristic that generally yields accurate results but
       * may not return the maximum score for a fingerprint (see ignored test cases).
       *
       * Scoring a fingerprint against a query is a harder problem as one would
       * intuitively think. An additional value in the fingerprint may require
       * reassignment of all previously matched values. Thus, probably the only
       * approach to yield an optimal result is to check all permutations of the
       * fingerprint.
       *
       * The following heuristic first orders the fingerprint by the maximum
       * achievable score of each individual term and uses this order to score
       * the fingerprint as a whole.
       */
      val termsByMaxPotentialScore =
        fingerprint.foldLeft((List[(String, Float)](), termScores)) {
          case ((acc, termScores), fp) =>
            termScores.getOrElse(fp, List(0f)) match {
              case x :: Nil =>
                ((fp -> x) :: acc, termScores - fp)
              case x :: rest =>
                ((fp -> x) :: acc, termScores + (fp -> rest))
              case Nil => ???
            }
        }._1.sortBy(-_._2).map(_._1)

      val (score, unmatchedTerms, scorer, scorePerValue) =
        termsByMaxPotentialScore.foldLeft((0f, 0, this, List[(String, Float)]())) {
          case ((score, unmatchedTerms, scorer, scorePerValue), fpt) =>
            scorer.score(fpt).fold {
              (score, unmatchedTerms + 1, scorer, scorePerValue)
            } {
              case (newScore, newScorer) =>
                (score + newScore, unmatchedTerms, newScorer, scorePerValue :+ (fpt -> newScore))
            }
        }

      val penalty = unmatchedTerms + scorer.unevaluatedBranches

      (score, penalty, scorePerValue)
    }

    def unevaluatedBranches: Float = this match {
      case SumNode(children) => children.map(_.unevaluatedBranches).sum
      case MaxNode(children) => children.map(_.unevaluatedBranches).sum / children.length
      case _: Leaf           => 1
      case DeadLeaf          => 0
    }

    lazy val termScores: Map[String, List[Float]] = {
      def rec(scorer: FingerprintScorer): Map[String, List[Float]] = scorer match {
        case Leaf(tpe, s) => Map(tpe -> List(s))
        case DeadLeaf     => Map()
        case InnerNode(children) =>
          children.flatMap(rec).foldLeft(Map[String, List[Float]]()) {
            case (acc, (tpe, scores)) =>
              acc + (tpe -> (scores ++ acc.getOrElse(tpe, Nil)))
          }
      }

      rec(this).mapValues(_.sortBy(-_))
    }

    /**
     * Calculates the score for a individual fingerprint type.
     *
     * If this node or one of the subnodes match `fpt` it returns some score
     * with a new scorer that wont match that particular leaf again.
     */
    def score(fpt: String): Option[(Float, FingerprintScorer)] = this match {
      case SumNode(children) =>
        (for {
          (child, idx) <- children.zipWithIndex
          (s, replacement) <- child.score(fpt)
        } yield (idx, s, replacement)) match {
          case Seq() => None
          case matches =>
            val (idx, score, newChild) = matches.maxBy(_._2)

            children.updated(idx, newChild).filter(_ != DeadLeaf) match {
              case Nil      => Some((score, DeadLeaf))
              case c :: Nil => Some((score, c))
              case cs       => Some((score, SumNode(cs)))
            }
        }
      case MaxNode(children) =>
        children.flatMap(_.score(fpt)) match {
          case Seq() => None
          case matches =>
            Some(matches.maxBy(_._1))
        }
      case Leaf(tpe, boost) =>
        if (tpe == fpt)
          Some((boost, DeadLeaf))
        else
          None
      case DeadLeaf =>
        None
    }

    override def toString: String = this match {
      case SumNode(cs)     => cs.mkString("sum(", ", ", ")")
      case MaxNode(cs)     => cs.mkString("max(", ", ", ")")
      case Leaf(fp, boost) => s"$fp^$boost"
      case DeadLeaf        => "∅"
    }
  }

  case class SumNode(children: List[FingerprintScorer]) extends FingerprintScorer
  case class MaxNode(children: List[FingerprintScorer]) extends FingerprintScorer
  case class Leaf(tpe: String, boost: Float) extends FingerprintScorer
  object DeadLeaf extends FingerprintScorer

  object InnerNode {
    def unapply(scorer: FingerprintScorer): Option[List[FingerprintScorer]] = scorer match {
      case SumNode(cs) => Some(cs)
      case MaxNode(cs) => Some(cs)
      case _           => None
    }
  }
}

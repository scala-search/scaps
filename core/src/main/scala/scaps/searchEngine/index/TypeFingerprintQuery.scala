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
import scaps.webapi.Contravariant
import scaps.webapi.Covariant

/**
 * A Lucene query that scores type fingerprints in a field against a type query.
 */
class TypeFingerprintQuery(field: String, apiQuery: ApiTypeQuery, subQuery: Query, frequencyCutoff: Double)
  extends CustomScoreQuery(
    TypeFingerprintQuery.matcherQuery(field, apiQuery, subQuery, frequencyCutoff),
    TypeFingerprintQuery.normFunctionQuery(field)) {

  val scorer = TypeFingerprintQuery.FingerprintScorer(apiQuery)

  override def name() = "TypeFingerprint"

  override def rewrite(reader: IndexReader) = this

  override def getCustomScoreProvider(context: AtomicReaderContext) =
    new CustomScoreProvider(context) {
      val reader = context.reader()

      override def customScore(doc: Int, subQueryScore: Float, valSrcScores: Array[Float]): Float =
        customScore(doc, subQueryScore, valSrcScores(0))

      override def customScore(doc: Int, subQueryScore: Float, normFromValSrc: Float): Float = {
        normFromValSrc * subQueryScore * score(doc)
      }

      def score(doc: Int): Float = {
        // There is probably a faster way to access the field value for every matched document.
        // Accessing the fingerprint through term vectors resulted in slightly worse performance.
        val fingerprint = reader.document(doc).getValues(field)

        scorer.score(fingerprint)
      }

      override def customExplain(doc: Int, subQueryExpl: Explanation, valSrcExpl: Explanation): Explanation = {
        customExplain(doc, subQueryExpl, Array(valSrcExpl))
      }

      override def customExplain(doc: Int, subQueryExpl: Explanation, valSrcExpls: Array[Explanation]): Explanation = {
        val normExplanation = valSrcExpls(0)
        val queryScore = score(doc)

        val expl = new Explanation(
          normExplanation.getValue * subQueryExpl.getValue * queryScore,
          "type fingerprint score, product of:")
        expl.addDetail(new Explanation(queryScore, "type fingerprint"))
        expl.addDetail(subQueryExpl)
        expl.addDetail(normExplanation)
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

    val terms = termsBelowCutoff(typeQuery, frequencyCutoff)

    logger.debug(s"Matching documents with fingerprint types: $terms")

    for {
      term <- terms
    } {
      fingerprintMatcher.add(new TermQuery(new Term(field, term)), Occur.SHOULD)
    }

    val matcherQuery = new BooleanQuery
    matcherQuery.add(new ConstantScoreQuery(fingerprintMatcher), Occur.SHOULD)
    matcherQuery.add(subQuery, Occur.SHOULD)

    matcherQuery
  }

  def termsBelowCutoff(typeQuery: ApiTypeQuery, frequencyCutoff: Double): List[String] = {
    val rankedTermsWithFreq = typeQuery.allTypes
      .sortBy(-_.boost)
      .map(t => (t.fingerprint, t.typeFrequency))
      .distinct

    val (terms, _) = rankedTermsWithFreq.foldLeft((List[String](), 0f)) {
      case (acc @ (accTerms, accFreq), (term, freq)) =>
        if (accFreq + freq < frequencyCutoff)
          (term :: accTerms, accFreq + freq)
        else
          acc
    }

    terms
  }

  /**
   * The norm function query is used to get access to the fingerprint norm value for all
   * matched documents.
   */
  def normFunctionQuery(field: String) = {
    new FunctionQuery(new NormValueSource(field))
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

    def score(fingerprint: Seq[String]): Float = {
      /*
       * This is just an heuristic that generally yields accurate results but
       * may not return the maximum score for a fingerprint (see ignored test cases).
       *
       * Scoring a fingerprint against a query is a harder problem as one would
       * intuitively think. An additional term in the fingerprint may require
       * reassignment of all previously matched terms. Thus, probably the only
       * approach to yield an optimal result is to check all permutations of the
       * fingerprint.
       *
       * The following heuristic first orders the fingerprint by the maximum
       * achievable score of each individual term and uses this order to score
       * the fingerprint as a whole.
       */
      val (termScores, preparedScorer) = prepare(fingerprint.distinct)

      val terms = {
        val maxScores = termScores.groupBy(_._1).mapValues(_.maxBy(_._2)._2)

        val termsWithMaxScore = fingerprint
          .flatMap(t => maxScores.get(t).map((t, _)))

        termsWithMaxScore
          .filter(_._2 > 0f)
          .sortBy(-_._2)
          .map(_._1)
      }

      terms.foldLeft((0f, FingerprintScorer.minimize(preparedScorer))) {
        case ((score, scorer), fpt) =>
          scorer.score(fpt).fold {
            (score, scorer)
          } {
            case (newScore, newScorer) => (score + newScore, newScorer)
          }
      }._1
    }

    /**
     * Collects all scores possible for each type in `fingerprint` and returns a new scorer
     * without leaves that wont match any of the types in `fingerprint`.
     */
    def prepare(fingerprint: Seq[String]): (Seq[(String, Float)], FingerprintScorer) = {
      def prepareChildren(fingerprint: Seq[String],
                          childNodes: List[FingerprintScorer],
                          mkNode: List[FingerprintScorer] => FingerprintScorer): (Seq[(String, Float)], FingerprintScorer) = {
        val childRes = childNodes.map(_.prepare(fingerprint))
        val childScores = childRes.flatMap(_._1)
        val matchingChilds = childRes.map(_._2).filter(_ != DeadLeaf)

        if (matchingChilds.isEmpty) {
          (Seq(), DeadLeaf)
        } else {
          (childScores, mkNode(matchingChilds))
        }
      }

      this match {
        case SumNode(children) => prepareChildren(fingerprint, children, SumNode(_))
        case MaxNode(children) => prepareChildren(fingerprint, children, MaxNode(_))
        case Leaf(tpe, boost) =>
          if (fingerprint.contains(tpe)) {
            (Seq(tpe -> boost), this)
          } else {
            (Seq(), DeadLeaf)
          }
        case DeadLeaf =>
          (Seq(), this)
      }
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
}

package scaps.searchEngine.index

import org.apache.lucene.queries.CustomScoreQuery
import scaps.searchEngine.ApiTypeQuery
import org.apache.lucene.search.BooleanQuery
import scaps.searchEngine.ApiQuery
import org.apache.lucene.search.TermQuery
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.ConstantScoreQuery
import org.apache.lucene.index.AtomicReaderContext
import org.apache.lucene.queries.CustomScoreProvider
import scala.collection.mutable.ListBuffer
import org.apache.lucene.index.TermsEnum
import org.apache.lucene.queries.function.FunctionQuery
import org.apache.lucene.queries.function.valuesource.ConstValueSource
import org.apache.lucene.index.IndexReader
import org.apache.lucene.search.Explanation
import org.apache.lucene.queries.function.valuesource.NormValueSource
import scaps.utils.Logging

class TypeFingerprintQuery(field: String, apiQuery: ApiTypeQuery)
  extends CustomScoreQuery(TypeFingerprintQuery.matcherQuery(field, apiQuery), TypeFingerprintQuery.normFunctionQuery(field)) {

  val scorer = TypeFingerprintQuery.FingerprintScorer(apiQuery)

  override def name() = "TypeFingerprint"

  override def rewrite(reader: IndexReader) = this

  override def getCustomScoreProvider(context: AtomicReaderContext) =
    new CustomScoreProvider(context) {
      override def customScore(doc: Int, subQueryScore: Float, valSrcScores: Array[Float]): Float =
        customScore(doc, subQueryScore, valSrcScores(0))

      override def customScore(doc: Int, subQueryScore: Float, normFromValSrc: Float): Float = {
        normFromValSrc * score(doc)
      }

      def score(doc: Int): Float = {
        val reader = context.reader()

        Option(reader.getTermVector(doc, field)).map { tv =>
          var terms: TermsEnum = null
          terms = tv.iterator(terms)

          val typesBuffer = new ListBuffer[String]
          var current = terms.next()

          while (current != null) {
            typesBuffer += current.utf8ToString()
            current = terms.next()
          }

          scorer.score(typesBuffer)
        }.getOrElse {
          throw new IllegalArgumentException(s"Field $field does not store term vectors.")
        }
      }

      override def customExplain(doc: Int, subQueryExpl: Explanation, valSrcExpl: Explanation): Explanation = {
        customExplain(doc, subQueryExpl, Array(valSrcExpl))
      }

      override def customExplain(doc: Int, subQueryExpl: Explanation, valSrcExpls: Array[Explanation]): Explanation = {
        val normExplanation = valSrcExpls(0)
        val queryScore = score(doc)

        val expl = new Explanation(normExplanation.getValue * queryScore, "type fingerprint score, product of:")
        expl.addDetail(new Explanation(queryScore, "type fingerprint"))
        expl.addDetail(normExplanation)
        expl
      }
    }
}

object TypeFingerprintQuery extends Logging {
  def matcherQuery(field: String, apiQuery: ApiTypeQuery) = {
    val q = new BooleanQuery

    val rankedTypesWithFp = apiQuery.allTypes
      .sortBy(-_.boost)
      .map(tpe => (tpe, tpe.fingerprint))

    val threshold = rankedTypesWithFp
      .find(typeWithTerm => thresholdTypes.contains(typeWithTerm._2))
      .map(_._1.boost)
      .getOrElse(0d)

    val terms = rankedTypesWithFp
      .takeWhile(_._1.boost > threshold)
      .take(10)
      .map(_._2)
      .distinct

    logger.debug(s"Matching documents with fingerprint types: $terms")

    for {
      term <- terms
    } {
      q.add(new TermQuery(new Term(field, term)), Occur.SHOULD)
    }
    new ConstantScoreQuery(q)
  }

  private val thresholdTypes = {
    import scaps.webapi._
    import scaps.webapi.TypeEntity._

    List(Any(Contravariant), AnyVal(Contravariant), AnyRef(Contravariant), Nothing(Covariant))
      .map(_.fingerprint)
  }

  def normFunctionQuery(field: String) = {
    new FunctionQuery(new NormValueSource(field))
  }

  object FingerprintScorer {
    def apply(q: ApiTypeQuery): FingerprintScorer =
      minimize(q match {
        case ApiTypeQuery.Sum(children) =>
          SumNode(children.map(apply))
        case ApiTypeQuery.Max(children) =>
          MaxNode(children.map(apply))
        case t @ ApiTypeQuery.Type(v, name, boost) =>
          Leaf(t.fingerprint, boost.toFloat)
      })

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
    def score(fpt: String): Option[(Float, FingerprintScorer)]

    def prepare(fingerprint: Seq[String]): (Seq[(String, Float)], FingerprintScorer)

    def score(documentFingerprint: Seq[String]): Float = {
      /*
       * This is only a heuristic that generally yields accurate results but
       * may not return the maximum score for a fingerprint (see ignored test cases).
       *
       * Scoring a fingerprint against a query is a harder problem as one would
       * intuitively think. An additional term in the fingerprint may require
       * reassignment of all previously matched terms. Thus, the only approach
       * to yield an optimal result is probably to check all permutations of the
       * fingerprint.
       *
       * The following heuristic first orders the fingerprint by the maximum
       * achievable score of each individual term and uses this order to score
       * the fingerprint as a whole.
       */
      val (termScores, preparedScorer) = prepare(documentFingerprint)

      val maxScores = termScores.groupBy(_._1).mapValues(_.maxBy(_._2)._2)

      val termsWithMaxScore = documentFingerprint
        .flatMap(t => maxScores.get(t).map((t, _)))

      val terms = termsWithMaxScore
        .filter(_._2 > 0f)
        .sortBy(-_._2)
        .map(_._1)

      terms.foldLeft((0f, preparedScorer)) {
        case ((score, scorer), fpt) =>
          scorer.score(fpt).fold((score, scorer)) {
            case (newScore, newScorer) => (score + newScore, newScorer)
          }
      }._1
    }

    val hasMatched: Boolean = false

    override def toString: String = this match {
      case SumNode(cs)     => cs.mkString("sum(", ", ", ")")
      case MaxNode(cs)     => cs.mkString("max(", ", ", ")")
      case Leaf(fp, boost) => s"$fp^$boost"
      case DeadLeaf        => "∅"
    }
  }

  case class SumNode(children: List[FingerprintScorer]) extends FingerprintScorer {
    def prepare(fingerprint: Seq[String]): (Seq[(String, Float)], FingerprintScorer) = {
      val childRes = children.map(_.prepare(fingerprint))
      val childScores = childRes.flatMap(_._1)
      val matchingChilds = childRes.map(_._2).filter(_ != DeadLeaf)

      if (matchingChilds.isEmpty) {
        (Seq(), DeadLeaf)
      } else {
        (childScores, SumNode(matchingChilds))
      }
    }

    implicit val o: Ordering[(Int, Float, FingerprintScorer)] = Ordering.Float.on(_._2)

    def score(fpt: String) =
      (for {
        (child, idx) <- children.zipWithIndex
        (s, replacement) <- child.score(fpt)
      } yield (idx, s, replacement)) match {
        case Seq() => None
        case matches =>
          val (idx, score, newChild) = matches.max

          children.updated(idx, newChild).filterNot(_.hasMatched) match {
            case Nil      => Some((score, DeadLeaf))
            case c :: Nil => Some((score, c))
            case cs       => Some((score, SumNode(cs)))
          }
      }
  }

  case class MaxNode(children: List[FingerprintScorer]) extends FingerprintScorer {
    def prepare(fingerprint: Seq[String]): (Seq[(String, Float)], FingerprintScorer) = {
      val childRes = children.map(_.prepare(fingerprint))
      val childScores = childRes.flatMap(_._1)
      val matchingChilds = childRes.map(_._2).filter(_ != DeadLeaf)

      if (matchingChilds.isEmpty) {
        (Seq(), DeadLeaf)
      } else {
        (childScores, MaxNode(matchingChilds))
      }
    }

    implicit val o: Ordering[(Float, FingerprintScorer)] = Ordering.Float.on(_._1)

    def score(fpt: String) =
      children.flatMap(_.score(fpt)) match {
        case Seq() => None
        case matches =>
          Some(matches.max)
      }
  }

  case class Leaf(tpe: String, boost: Float) extends FingerprintScorer {
    def prepare(fingerprint: Seq[String]): (Seq[(String, Float)], FingerprintScorer) =
      if (fingerprint.contains(tpe)) {
        (Seq(tpe -> boost), this)
      } else {
        (Seq(), DeadLeaf)
      }

    def score(fpt: String) =
      if (tpe == fpt)
        Some((boost, DeadLeaf))
      else
        None
  }

  object DeadLeaf extends FingerprintScorer {
    def prepare(fingerprint: Seq[String]): (Seq[(String, Float)], FingerprintScorer) = (Seq(), this)

    def score(fpt: String) = None

    override val hasMatched = true
  }
}

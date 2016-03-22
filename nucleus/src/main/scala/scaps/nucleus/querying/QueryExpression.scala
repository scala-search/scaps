package scaps.nucleus.querying

import scaps.nucleus.indexing.FingerprintTerm

private[nucleus] sealed trait QueryExpression {
  import QueryExpression._

  def score(fingerprint: List[FingerprintTerm]): (Float, Float, List[(String, Float)]) = {
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
    val termsWithIsOpt = fingerprint.map(t => (t.key, t.isOptional))

    val termsByMaxPotentialScore: List[(String, Boolean)] =
      termsWithIsOpt.foldLeft((List[(String, Boolean, Float)](), termScores)) {
        case ((acc, termScores), (fp, isOpt)) =>
          termScores.getOrElse(fp, List(0f)) match {
            case x :: Nil =>
              ((fp, isOpt, x) :: acc, termScores - fp)
            case x :: rest =>
              ((fp, isOpt, x) :: acc, termScores + (fp -> rest))
            case Nil => ???
          }
      }._1
        .sortBy { case (_, _, maxScore) => -maxScore }
        .map(t => (t._1, t._2))

    val (score, unmatchedTerms, reducedExpr, scorePerValue) =
      termsByMaxPotentialScore.foldLeft((0f, 0f, this, List[(String, Float)]())) {
        case ((score, unmatchedTerms, expr, scorePerValue), (fpt, isOpt)) =>
          expr.score(fpt).fold {
            val unmatched = unmatchedTerms + (if (isOpt) 0.1f else 1f)
            (score, unmatched, expr, scorePerValue)
          } {
            case (newScore, newExpr) =>
              (score + newScore, unmatchedTerms, newExpr, scorePerValue :+ (fpt -> newScore))
          }
      }

    val penalty = unmatchedTerms + reducedExpr.unevaluatedBranches

    (score, penalty, scorePerValue)
  }

  def unevaluatedBranches: Float = this match {
    case Sum(children) => children.map(_.unevaluatedBranches).sum
    case Max(children) => children.map(_.unevaluatedBranches).sum / children.length
    case _: Leaf       => 1
    case DeadLeaf      => 0
  }

  lazy val termScores: Map[String, List[Float]] = {
    def rec(scorer: QueryExpression): Map[String, List[Float]] = scorer match {
      case Leaf(tpe, s, _) => Map(tpe -> List(s))
      case DeadLeaf        => Map()
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
  def score(fpt: String): Option[(Float, QueryExpression)] = this match {
    case Sum(children) =>
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
            case cs       => Some((score, Sum(cs)))
          }
      }
    case Max(children) =>
      children.flatMap(_.score(fpt)) match {
        case Seq() => None
        case matches =>
          Some(matches.maxBy(_._1))
      }
    case Leaf(tpe, boost, _) =>
      if (tpe == fpt)
        Some((boost, DeadLeaf))
      else
        None
    case DeadLeaf =>
      None
  }

  def leaves: List[QueryExpression.Leaf] = this match {
    case InnerNode(cs) => cs.flatMap(_.leaves)
    case l: Leaf       => l :: Nil
  }

  def termsBelowCutoff(frequencyCutoff: Double): List[String] = {
    val rankedTermsWithFreq = leaves
      .sortBy(-_.score)
      .map(t => (t.term, t.frequency))
      .distinct

    val (terms, _) = rankedTermsWithFreq.foldLeft((List[String](), 0d)) {
      case (acc @ (accTerms, accFreq), (value, freq)) =>
        if (accFreq + freq < frequencyCutoff)
          (value :: accTerms, accFreq + freq)
        else
          acc
    }

    terms
  }

  override def toString: String = this match {
    case Sum(cs)                    => cs.mkString("sum(", ", ", ")")
    case Max(cs)                    => cs.mkString("max(", ", ", ")")
    case Leaf(fp, boost, frequency) => s"$fp^($boost, $frequency)"
    case DeadLeaf                   => "∅"
  }

}

private[nucleus] object QueryExpression {
  case class Sum(children: List[QueryExpression]) extends QueryExpression
  case class Max(children: List[QueryExpression]) extends QueryExpression
  case class Leaf(term: String, score: Float, frequency: Float) extends QueryExpression
  case object DeadLeaf extends QueryExpression

  object InnerNode {
    def unapply(expr: QueryExpression): Option[List[QueryExpression]] = expr match {
      case Sum(cs) => Some(cs)
      case Max(cs) => Some(cs)
      case _       => None
    }
  }
}

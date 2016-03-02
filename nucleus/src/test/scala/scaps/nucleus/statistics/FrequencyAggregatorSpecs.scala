package scaps.nucleus.statistics

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.nucleus.DefBuilder._
import scaps.nucleus.TestLanguage._
import scaps.nucleus.indexing.Indexer
import scaps.nucleus.IndexSettings
import scaps.nucleus.IndexAccess
import scaps.nucleus.Document
import scaps.nucleus.indexing.TypeNormalization
import scaps.nucleus.Definition

class FrequencyAggregatorSpecs extends FlatSpec with Matchers {
  def tf(defs: List[Definition]) = {

    val docs = defs.flatMap(Indexer.defToDocs)

    val aggregator = new FrequencyAggregator(testModel, new IndexAccess {
      override def getByKeys(keys: Seq[String]) = docs.filter(d => keys.forall(d.keys.contains(_)))
    })

    val tfs = aggregator.typeFrequencies()

    assertElementaryRules(tfs)

    tfs
  }

  def assertElementaryRules(tfs: Map[String, Int]) = {
    val tfTop0 = tfs("-<top0>")
    val tfBottom0 = tfs("+<bottom0>")
    val tfUnknown0 = tfs("/<unknown0>")

    assert(tfs.filter(_._1.startsWith("-")).map(_._2).forall(_ <= tfTop0))
    assert(tfs.filter(_._1.startsWith("+")).map(_._2).forall(_ <= tfBottom0))
    assert(tfs.map(_._2).forall(_ <= tfUnknown0))
  }

  it should "collect frequencies at covariant positions" in {
    val defs = List(
      deff()(+T.Int),
      deff()(+T.Long),
      extendss(+T.Long),
      extendss(+T.Int, +T.Long))

    tf(defs) should contain allOf (
      "+Int" -> 2, "/Int" -> 1, "+Long" -> 1, "/Long" -> 1, "+<bottom0>" -> 2)
  }

  it should "collect frequencies at contravariant positions" in {
    val defs = List(
      deff(-T.Int)(+T.Unit),
      deff(-T.Long)(+T.Unit),
      extendss(+T.Long),
      extendss(+T.Int, +T.Long))

    tf(defs) should contain allOf (
      "-Int" -> 1, "/Int" -> 1, "-Long" -> 2, "/Long" -> 1, "-<top0>" -> 2)
  }

  it should "collect frequencies at invariant positions" in {
    val defs = List(
      deff(-T.Array(~T.Int))(+T.Unit),
      deff(-T.Array(~T.Long))(+T.Unit),
      extendss(+T.Long),
      extendss(+T.Int, +T.Long))

    tf(defs) should contain allOf (
      "/Int" -> 1, "/Long" -> 1, "/<unknown0>" -> 2)
  }
}

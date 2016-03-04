package scaps.nucleus.indexing

import scaps.nucleus.Document
import scaps.nucleus.MetaDoc
import scaps.nucleus.TypeDef
import scaps.nucleus.IndexAccess

object TypeFrequencyIndex {

  val typeFrequencyKey = "<tf>"

  def typeFrequencyDocs(index: IndexAccess): Iterable[Document] = {
    typeFrequencies(index).map {
      case (term, count) =>
        MetaDoc(List(s"$typeFrequencyKey:$term"), upickle.default.write(count).getBytes, "")
    }
  }

  def typeFrequencies(index: IndexAccess): Map[String, Int] =
    typeFrequencies(
      index,
      TypeDefIndex.allTypeDefs(index),
      TypeViewIndex.allViews(index))

  def typeFrequencies(index: IndexAccess, types: Seq[TypeDef], views: Seq[TypeView]): Map[String, Int] = {
    val elementary = types.flatMap(t => TypeView.elementaryTypeViews(t))
    val allViews = elementary ++ views

    val viewsByFrom = allViews.groupBy(_.from).toIterable

    viewsByFrom.flatMap {
      case (from, views) =>
        val fp = Fingerprint(from).map(_.key)

        val noMatchingDocs = index.countByKeys(fp)

        if (noMatchingDocs > 0)
          views.flatMap { v =>
            val toFp = Fingerprint(v.to).map(_.key)
            toFp.map(term => (term, noMatchingDocs))
          }
        else
          Nil
    }.foldLeft(Map[String, Int]().withDefaultValue(0)) { (acc, tf) =>
      val prev = acc(tf._1)
      acc + (tf._1 -> (tf._2 + prev))
    }
  }
}

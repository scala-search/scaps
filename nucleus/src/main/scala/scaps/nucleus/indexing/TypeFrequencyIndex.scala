/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.indexing

import scaps.nucleus.Document
import scaps.nucleus.MetaDoc
import scaps.nucleus.TypeDef
import scaps.nucleus.IndexAccess
import scaps.nucleus.Covariant

private[nucleus] object TypeFrequencyIndex {

  private val typeFrequencyKey = "<tf>"

  private val maxTfTerm = FingerprintTerm(Covariant, InternalTypes.Bottom().name)

  def typeFrequency(index: IndexAccess)(term: FingerprintTerm): Option[Int] = {
    index.getByKeys(Seq(key(term))).headOption.map { doc =>
      upickle.default.read[Int](new String(doc.data))
    }
  }

  def relativeTermFrequency(index: IndexAccess)(term: FingerprintTerm): Float = {
    (for {
      maxTf <- typeFrequency(index)(maxTfTerm)
      tf <- typeFrequency(index)(term)
    } yield (tf.toFloat / maxTf.toFloat)).getOrElse(Float.MinPositiveValue)
  }

  private def key(term: FingerprintTerm): String = {
    s"$typeFrequencyKey:${term.key}"
  }

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
    val elementary = types.flatMap(t => TypeView.elementaryTypeViews(t.tpe))
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

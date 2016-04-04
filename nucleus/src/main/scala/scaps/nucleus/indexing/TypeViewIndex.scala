/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.indexing

import scaps.nucleus.Definition
import scaps.nucleus.Document
import scaps.nucleus.MetaDoc
import scaps.nucleus.TypeRef
import scaps.nucleus.IndexAccess
import scaps.nucleus.TypeParam
import scaps.nucleus.Type

private[nucleus] object TypeViewIndex {
  import scaps.nucleus.indexing.{ InternalTypes => I }

  private val viewKey = "<v>"

  def defToDocs(d: Definition): List[Document] =
    TypeView.typeViews(d).map(v => typeViewToDoc(d.source, v))

  private def typeViewToDoc(source: String, v: TypeView): Document = {
    val data = upickle.default.write(v).getBytes

    MetaDoc(List(viewKey, key(v.from)), data, source)
  }

  private def docToTypeView(doc: Document): Option[TypeView] = {
    if (doc.keys.contains(viewKey))
      Some(upickle.default.read[TypeView](new String(doc.data)))
    else
      None
  }

  def resolveTransitiveViews(newViews: Seq[TypeView], index: IndexAccess): Seq[Document] = {
    // TODO
    ???
  }

  private def key(tr: TypeRef): String =
    s"$viewKey:${tr.variance.prefix}${tr.name}"

  def allViews(index: IndexAccess): Seq[TypeView] =
    index.getByKeys(Seq(viewKey)).flatMap(docToTypeView)

  private def findViewsFrom(tpe: Type, index: IndexAccess): Seq[TypeView] = {
    val docs = index.getByKeys(Seq(viewKey, key(tpe.ref)))
    docs.flatMap(docToTypeView)
  }

  def viewsFrom(tpe: Type, index: IndexAccess): Seq[TypeView] =
    TypeView.elementaryTypeViews(tpe) ++ findViewsFrom(tpe, index)

  def typesViewableFrom(tpe: Type, index: IndexAccess): Seq[TypeRef] =
    TypeView.elementaryAlternatives(tpe.ref) ++
      findViewsFrom(tpe, index).flatMap(_.apply(tpe.ref))
}

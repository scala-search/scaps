/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.indexing

import scaps.nucleus.Document
import scaps.nucleus.MetaDoc
import scaps.nucleus.TypeDef
import scaps.nucleus.IndexAccess

private[nucleus] object TypeDefIndex {
  private val typeDefKey = "<t>"

  def typeDefToDoc(td: TypeDef): Document =
    MetaDoc(
      List(typeDefKey),
      upickle.default.write(td).getBytes,
      td.source)

  private def docToTypeDef(doc: Document): Option[TypeDef] =
    if (doc.keys.contains(typeDefKey))
      Some(upickle.default.read[TypeDef](new String(doc.data)))
    else
      None

  def allTypeDefs(index: IndexAccess): Seq[TypeDef] =
    index.getByKeys(Seq(typeDefKey)).flatMap(docToTypeDef)
}

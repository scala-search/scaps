package scaps.nucleus.indexing

import scaps.nucleus.Document
import scaps.nucleus.MetaDoc
import scaps.nucleus.TypeDef
import scaps.nucleus.IndexAccess

object TypeDefIndex {
  val typeDefKey = "<t>"

  def typeDefToDoc(td: TypeDef): Document =
    MetaDoc(
      List(typeDefKey),
      upickle.default.write(td).getBytes,
      td.source)

  def docToTypeDef(doc: Document): Option[TypeDef] =
    if (doc.keys.contains(typeDefKey))
      Some(upickle.default.read[TypeDef](new String(doc.data)))
    else
      None

  def allTypeDefs(index: IndexAccess): Seq[TypeDef] =
    index.getByKeys(Seq(typeDefKey)).flatMap(docToTypeDef)
}

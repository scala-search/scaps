package scaps.nucleus.indexing

import scaps.nucleus.ValueDef
import scaps.nucleus.Document
import scaps.nucleus.ValueDoc

object ValueIndex {
  def valueToDoc(v: ValueDef): Document =
    ValueDoc(
      v.name,
      Fingerprint(v).map(_.key),
      upickle.default.write(v).getBytes,
      v.source)
}

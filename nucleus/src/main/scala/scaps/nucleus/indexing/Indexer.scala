package scaps.nucleus.indexing

import scaps.nucleus.Definition
import scaps.nucleus.Document
import scaps.nucleus.ValueDef
import scaps.nucleus.MetaDoc
import scaps.nucleus.ValueDoc
import scaps.nucleus.TypeDef
import upickle._
import scaps.nucleus.TypeRef

private[nucleus] object Indexer {

  def defToDocs(d: Definition): List[Document] = {
    d match {
      case v: ValueDef =>
        val fp = mkValueDoc(v, Fingerprint(v))
        val views = TypeView.typeViews(v).map(mkViewDoc(v, _))

        fp :: views
      case s: TypeDef =>
        mkTypeDoc(s) ::
          TypeView.typeViews(s).map(mkViewDoc(s, _))
    }
  }

  def mkValueDoc(v: ValueDef, fp: List[FingerprintTerm]): Document = {
    val data = write(fp).getBytes

    ValueDoc(v.name, fp.map(_.key), data, v.source)
  }

  def mkTypeDoc(s: TypeDef): Document = {
    val data = write(s).getBytes

    MetaDoc(List("<t>"), data, s.source)
  }

  def mkViewDoc(d: Definition, v: TypeView): Document = {
    val data = write(v).getBytes

    MetaDoc(List("<v>", s"<v>:${v.from}"), data, d.source)
  }

  def viewKeys(t: TypeRef): List[String] = {
    //-List[-Int]
    //-List[-Int], -List[-_]
    //-Map[\Int, -Char]
    //-Map[\Int, -Char], -Map[\_, -_]
    ???
  }
}

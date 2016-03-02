package scaps.nucleus.indexing

import scaps.nucleus.Definition
import scaps.nucleus.Document
import scaps.nucleus.MetaDoc
import upickle._
import scaps.nucleus.TypeRef
import scaps.nucleus.IndexAccess
import scaps.nucleus.TypeParam

private[nucleus] object TypeViewIndex {
  val viewKey = "<v>"

  def typeViewToDoc(source: String, v: TypeView): Document = {
    val data = write(v).getBytes

    MetaDoc(List(viewKey, s"${v.from}"), data, source)
  }

  def docToTypeView(doc: Document): Option[TypeView] = {
    if (doc.keys.contains(viewKey))
      Some(read[TypeView](new String(doc.data)))
    else
      None
  }

  def resolveTransitiveViews(newViews: Seq[TypeView], index: IndexAccess): Seq[Document] = ???

  def matchingTypeKeys(t: TypeRef): Seq[String] = {
    val key = t.toString

    //-List[-_]
    //-List[-Int]
    //-List[(-Int, -Int)], -List[(-_, -_)]
    //-Int
    // TODO
    Seq(key)
  }

  def allViews(index: IndexAccess): Seq[TypeView] =
    index.getByKeys(Seq(viewKey)).flatMap(docToTypeView)

  def findViewsFrom(params: List[TypeParam], t: TypeRef, index: IndexAccess): Seq[TypeView] = {
    val keyss = matchingTypeKeys(t).map(key => Seq(viewKey, key))
    val docs = index.getManyByKeys(keyss)
    docs.flatMap(docToTypeView)
  }
}

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

  val viewKey = "<v>"

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
    s"$viewKey:$tr"

  private def matchingTypeKeys(tpe: Type): Seq[String] = {
    val normedParams = TypeNormalization.renameTypeParams(tpe.params.map(p => (p, I.__().name)), tpe.ref)
    //-List[-_]
    //-List[-Int]
    //-List[(-Int, -Int)], -List[(-_, -_)]
    //-Int
    // TODO
    Seq(key(normedParams))
  }

  def allViews(index: IndexAccess): Seq[TypeView] =
    index.getByKeys(Seq(viewKey)).flatMap(docToTypeView)

  private def findViewsFrom(tpe: Type, index: IndexAccess): Seq[TypeView] = {
    val keyss = matchingTypeKeys(tpe).map(key => Seq(viewKey, key))

    val docs = index.getManyByKeys(keyss)
    docs.flatMap(docToTypeView)
  }

  def typesViewableFrom(tpe: Type, index: IndexAccess): Seq[TypeRef] =
    TypeView.elementaryAlternatives(tpe.ref) ++
      findViewsFrom(tpe, index).flatMap(_.apply(tpe.ref))
}

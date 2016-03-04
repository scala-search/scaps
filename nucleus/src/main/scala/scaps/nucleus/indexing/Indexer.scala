package scaps.nucleus.indexing

import scaps.nucleus.Definition
import scaps.nucleus.Document
import scaps.nucleus.ValueDef
import scaps.nucleus.MetaDoc
import scaps.nucleus.ValueDoc
import scaps.nucleus.TypeDef
import scaps.nucleus.TypeRef
import scaps.nucleus.LanguageSettings

private[nucleus] object Indexer {
  def defToDocs(d: Definition, language: LanguageSettings): List[Document] = {
    val internal = InternalTypes.toInternal(d, language)

    val defDoc = internal match {
      case v: ValueDef =>
        ValueIndex.valueToDoc(v)
      case t: TypeDef =>
        TypeDefIndex.typeDefToDoc(t)
    }

    defDoc :: TypeViewIndex.defToDocs(internal)
  }
}

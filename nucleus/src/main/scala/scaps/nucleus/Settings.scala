package scaps.nucleus

import java.util.regex.Pattern

case class Settings(
  language: LanguageSettings,
  index: IndexSettings,
  query: QuerySettings)

case class LanguageSettings(
  topTypePattern: Pattern,
  bottomTypePattern: Pattern,

  repeatedType: Option[String],

  functionTypePattern: Pattern)

case class IndexSettings()

object IndexSettings {
  val default = IndexSettings()
}

case class QuerySettings()

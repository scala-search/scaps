package scaps.nucleus

import scaps.nucleus.indexing.InternalTypes;
import scala.language.implicitConversions

import java.util.regex.Pattern

object TestLanguage {
  val testModel = LanguageSettings(
    topTypePattern = Pattern.compile("Any"),
    bottomTypePattern = Pattern.compile("Nothing"),
    repeatedType = None,
    functionTypePattern = Pattern.compile("""(Fn[0-9]+)|((<memberAccess|<methodInvocation)[0-9]+>)"""))

  object A extends InternalTypes.Type("A")

  object T {
    object Any extends InternalTypes.Type("Any")
    object Nothing extends InternalTypes.Type("Nothing")

    object Int extends InternalTypes.Type("Int")
    object Long extends InternalTypes.Type("Long")
    object String extends InternalTypes.Type("String")
    object Unit extends InternalTypes.Type("Unit")

    object Seq extends InternalTypes.UnaryType("Seq")
    object List extends InternalTypes.UnaryType("List")
    object Array extends InternalTypes.UnaryType("Array")

    object Fn extends InternalTypes.FunctionLikeType("Fn", "")

    object MemberAccess extends InternalTypes.FunctionLikeType("<memberAccess")
    object MethodInvocation extends InternalTypes.FunctionLikeType("<methodInvocation")
    object Repeated extends InternalTypes.UnaryType("<repeated>")
  }
}

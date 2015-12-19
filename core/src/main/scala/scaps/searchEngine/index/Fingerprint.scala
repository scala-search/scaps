package scaps.searchEngine.index

import scaps.api.ValueDef
import scaps.api.TypeRef

case class Fingerprint(terms: List[(String, Boolean)]) extends AnyVal {
  override def toString =
    termStrings.mkString(" ")

  def termStrings: List[String] = {
    def termStr(term: (String, Boolean)) =
      (if (term._2) "?" else "!") + term._1

    terms.map(termStr)
  }
}

object Fingerprint {
  def apply(v: ValueDef): Fingerprint = {
    Fingerprint(terms(v.tpe.normalize(v.typeParameters)).sortBy(_._1))
  }

  def terms(t: TypeRef, inImplicit: Boolean = false): List[(String, Boolean)] = t match {
    case TypeRef.Ignored(args, _)         => args.flatMap(terms(_, inImplicit))
    case TypeRef.Implicit(arg, _)         => terms(arg, true)
    case TypeRef(name, variance, args, _) => (variance.prefix + name, inImplicit) :: args.flatMap(terms(_, inImplicit))
  }

  def apply(s: String): Fingerprint =
    Fingerprint(termsFromString(s))

  def termsFromString(s: String): List[(String, Boolean)] = {
    var newTerm = true
    var requiredTerm = false
    var currName = new StringBuilder
    var buff = List.newBuilder[(String, Boolean)]
    s.foreach { c =>
      if (newTerm) {
        c match {
          case '!' => requiredTerm = true
          case '?' => requiredTerm = false
        }
        newTerm = false
      } else {
        c match {
          case ' ' =>
            buff += ((currName.result(), requiredTerm))
            currName.clear()
            newTerm = true
          case c =>
            currName += c
        }
      }
    }
    buff += ((currName.result(), requiredTerm))
    buff.result()
  }
}

package scaps.searchEngine.index

import scaps.webapi._
import scala.collection.mutable.MapBuilder
import scaps.utils.printval

class TypeFrequencyAccumulator(
  terms: Seq[TermEntity],
  baseTypes: String => Seq[TypeEntity],
  subClasses: TypeEntity => Seq[ClassEntity]) {

  def apply(): Map[(Variance, String), Int] = {
    val entries = terms.flatMap(getEntries)

    entries
      .groupBy(identity)
      .mapValues(_.length)
      .withDefaultValue(0)
  }

  def getEntries(term: TermEntity): Set[(Variance, String)] =
    typesInTerm(term).flatMap(getEntries)

  def typesInTerm(term: TermEntity): Set[TypeEntity] = {
    def removeArgs(tpe: TypeEntity) =
      tpe.copy(args = Nil)

    def removeNestedArgs(tpe: TypeEntity) =
      tpe.copy(args = tpe.args.map(removeArgs))

    def rec(tpe: TypeEntity): Set[TypeEntity] = tpe match {
      case TypeEntity("", _, _) =>
        Set()
      case TypeEntity.MemberAccess(owner, member) =>
        rec(owner) ++ rec(member)
      case TypeEntity.MethodInvocation(args, res, _) =>
        rec(res) ++ args.flatMap(rec)
      case t @ TypeEntity(name, v @ (Contravariant | Invariant), args) =>
        args.toSet.flatMap(rec) + removeArgs(t)
      case t @ TypeEntity(_, Covariant, args) =>
        args.toSet.flatMap(rec) + removeNestedArgs(t)
    }

    rec(term.tpe.renameTypeParams(term.typeParameters, _ => "_"))
  }

  def getEntries(tpe: TypeEntity): Seq[(Variance, String)] =
    tpe.variance match {
      case Covariant =>
        (tpe +: TypeEntity.Nothing.cls +: subClasses(tpe)).map(t => (Covariant, t.name))
      case Contravariant =>
        (tpe +: baseTypes(tpe.name)).map(t => (Contravariant, t.name))
      case Invariant => Seq((Invariant, tpe.name), (Invariant, TypeEntity.Unknown.name))
    }
}

object TypeFrequencyAccumulator {
  def apply(termIndex: TermsIndex, classIndex: ClassIndex) = {
    val getBaseTypes = (name: String) =>
      classIndex.findClassBySuffix(name).get.flatMap(_.baseTypes)

    val getSubClasses = Memo {
      (tpe: TypeEntity) => classIndex.findStrictSubclass(tpe).get
    }

    new TypeFrequencyAccumulator(
      termIndex.allTerms().get,
      getBaseTypes,
      getSubClasses)
  }

  case class Memo[T, R](f: T => R) extends (T => R) {
    val results = scala.collection.mutable.Map[T, R]()

    def apply(t: T) = {
      results.get(t).fold {
        val r = f(t)
        results += (t -> r)
        r
      } { identity }
    }
  }
}

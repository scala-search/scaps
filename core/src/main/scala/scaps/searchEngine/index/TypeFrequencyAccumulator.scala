package scaps.searchEngine.index

import scaps.webapi._
import scala.collection.mutable.MapBuilder
import scaps.utils.printval

class TypeFrequencyAccumulator(
  terms: Seq[TermEntity],
  baseTypes: String => Seq[TypeEntity],
  subClasses: TypeEntity => Seq[ClassEntity]) {

  def apply(): Map[(Variance, String), Int] = {
    val types = terms.foldLeft(List[TypeEntity]()) { (tpes, term) =>
      val termTypes = typesInTerm(term)

      tpes ++ termTypes
    }

    val entriesWithCount = types.groupBy(identity).view.flatMap {
      case (tpe, tpes) => getEntries(tpe).map((_, tpes.length))
    }

    entriesWithCount
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)
      .withDefaultValue(0)
  }

  def typesInTerm(term: TermEntity): Seq[TypeEntity] = {
    def rec(tpe: TypeEntity): Seq[TypeEntity] = tpe match {
      case TypeEntity.MemberAccess(owner, member)    => rec(owner) ++ rec(member)
      case TypeEntity.MethodInvocation(args, res, _) => args.flatMap(rec(_)) ++ rec(res)
      case t                                         => t.toList
    }

    rec(term.tpe).distinct
  }

  def getEntries(tpe: TypeEntity): Seq[(Variance, String)] = tpe.variance match {
    case Covariant =>
      (tpe +: TypeEntity.Nothing.cls +: subClasses(tpe)).map(t => (Covariant, t.name))
    case Contravariant =>
      (tpe +: baseTypes(tpe.name)).map(t => (Contravariant, t.name))
    case v => Seq((v, tpe.name))
  }
}

object TypeFrequencyAccumulator {
  def apply(termIndex: TermsIndex, classIndex: ClassIndex) = {
    val getBaseTypes = (name: String) =>
      classIndex.findClassBySuffix(name).get.flatMap(_.baseTypes)

    val getSubClasses = (tpe: TypeEntity) =>
      classIndex.findSubClasses(tpe).get

    new TypeFrequencyAccumulator(
      termIndex.allTerms().get,
      getBaseTypes,
      getSubClasses)
  }
}

package scaps.searchEngine.index

import scaps.webapi._
import scala.collection.mutable.MapBuilder
import scaps.utils.printval

class TypeFrequencyAccumulator(
  terms: Seq[TermEntity],
  baseTypes: String => Seq[TypeEntity],
  subClasses: TypeEntity => Seq[ClassEntity]) {

  def apply(): Map[(Variance, String), Int] = {
    val typesB = Seq.newBuilder[TypeEntity]

    terms.foreach { term =>
      typesB ++= typesInTerm(term)
    }

    val types = typesB.result()

    //    val mapB = Map.newBuilder[(Variance, String), Int]
    //
    //    typesB.result().groupBy(identity).foreach{
    //      case (tpe, tpes) =>
    //        val entries = getEntries(tpe).map((_, tpes.length))
    //        entries.groupBy(_._1).foreach{
    //          case (entry, entries) =>
    //            val count = entries.map(_._2).sum
    //
    //
    //        }
    //    }

    val entriesWithCount = types.groupBy(identity).view.flatMap {
      case (tpe, tpes) => getEntries(tpe).map((_, tpes.length))
    }

    entriesWithCount
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)
      .withDefaultValue(0)
  }

  def typesInTerm(term: TermEntity): Set[TypeEntity] = {
    def rec(tpe: TypeEntity): Set[TypeEntity] = tpe match {
      case TypeEntity("", _, _) =>
        Set()
      case TypeEntity.MemberAccess(owner, member) =>
        rec(owner) ++ rec(member)
      case TypeEntity.MethodInvocation(args, res, _) =>
        rec(res) ++ args.flatMap(rec)
      case TypeEntity.Refinement(args, Covariant) =>
        args.toSet.flatMap(rec)
      case TypeEntity(name, v @ (Contravariant | Invariant), args) =>
        args.toSet.flatMap(rec) + TypeEntity(name, v, Nil)
      case t @ TypeEntity(_, Covariant, args) =>
        args.toSet.flatMap(rec) + t
    }

    rec(term.tpe.renameTypeParams(term.typeParameters, _ => ""))
  }

  def getEntries(tpe: TypeEntity): Seq[(Variance, String)] =
    scaps.utils.printval(s"entries for $tpe",
      tpe.variance match {
        case Covariant =>
          (tpe +: TypeEntity.Nothing.cls +: subClasses(tpe)).map(t => (Covariant, t.name))
        case Contravariant =>
          (tpe +: baseTypes(tpe.name)).map(t => (Contravariant, t.name))
        case v => Seq((v, tpe.name))
      })
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

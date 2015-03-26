package scala.tools.apiSearch.featureExtraction

import scala.tools.nsc.interactive.Global
import scala.tools.apiSearch.model._

trait EntityFactory {
  val compiler: Global

  import compiler._

  def extractEntities(classSym: Symbol, getDocComment: Symbol => String): List[Entity] = {
    if (isClassOfInterest(classSym)) {
      val cls = createClassEntity(classSym)

      val objTerm =
        if (isTermOfInterest(classSym)) createTermEntity(classSym, getDocComment(classSym)) :: Nil
        else Nil

      val memberSyms = classSym.tpe.decls
        .filter(isTermOfInterest)

      val referencedClasses = memberSyms.flatMap { sym =>
        sym.tpe.collect { case t => t.typeSymbol }
          .filter(isClassOfInterest _)
          .map(createClassEntity _)
      }.toList

      val members = memberSyms
        .map(sym => createTermEntity(sym, getDocComment(sym)))
        .toList

      cls :: objTerm ::: members ::: referencedClasses
    } else {
      Nil
    }
  }

  def createClassEntity(sym: Symbol): ClassEntity = {
    val baseTypes = sym.tpe.baseTypeSeq.toList.tail
      .filter(tpe => isClassOfInterest(tpe.typeSymbol))
      .map(tpe => createTypeEntity(tpe, Covariant))
    ClassEntity(qualifiedName(sym), typeParamsFromOwningTemplates(sym), baseTypes)
  }

  def isClassOfInterest(sym: Symbol): Boolean =
    (sym.isClass || sym.isModuleOrModuleClass) &&
      !sym.isAnonOrRefinementClass &&
      !sym.isLocalClass

  def createTermEntity(sym: Symbol, rawComment: String): TermEntity = {
    val (typeParams, tpe) = createTypeEntity(sym)
    TermEntity(qualifiedName(sym), typeParams, tpe, rawComment)
  }

  def isTermOfInterest(sym: Symbol): Boolean =
    (sym.isTerm || (sym.isConstructor && !sym.owner.isAbstractClass)) &&
      sym.isPublic

  def createTypeEntity(sym: Symbol): (List[TypeParameterEntity], TypeEntity) = {
    val (params, memberType) =
      if (sym.isMethod)
        methodType(sym)
      else
        (Nil, createTypeEntity(sym.tpe, Covariant))

    if (sym.owner.isClass && !sym.owner.isModuleClass) {
      (typeParamsFromOwningTemplates(sym) ++ params,
        TypeEntity.memberAccess(TypeEntity(qualifiedName(sym.owner), Contravariant, Nil), memberType))
    } else {
      (params, memberType)
    }
  }

  private def typeParamsFromOwningTemplates(sym: Symbol): List[TypeParameterEntity] = {
    sym.ownerChain.reverse.flatMap { owner =>
      owner.tpe.typeArgs.map(arg => createTypeParamEntity(arg.typeSymbol))
    }
  }

  def qualifiedName(sym: Symbol): String = {
    def name(sym: Symbol) = sym.name.toString
    def rec(sym: Symbol): String =
      if (sym.isRootSymbol || sym == sym.owner) ""
      else if (sym.hasPackageFlag || sym.hasModuleFlag) rec(sym.owner) + name(sym) + "."
      else rec(sym.owner) + name(sym) + "#"

    if (sym.isTypeParameter)
      name(sym)
    else
      rec(sym.owner) + name(sym)
  }

  private def createTypeEntity(tpe: Type, variance: Variance): TypeEntity = {
    def getVariance(idx: Int) = {
      val nscVariance = tpe.typeSymbol.typeParams(idx).variance
      if (nscVariance.isPositive)
        variance
      else if (nscVariance.isContravariant)
        variance.flip
      else
        Invariant
    }

    val args = tpe.typeArgs.zipWithIndex.map {
      case (arg, idx) => createTypeEntity(arg, getVariance(idx))
    }
    TypeEntity(qualifiedName(tpe.typeSymbol), variance, args)
  }

  private def methodType(sym: Symbol): (List[TypeParameterEntity], TypeEntity) = {
    val typeParams = sym.tpe.typeParams.map(createTypeParamEntity)

    def rec(paramss: List[List[Symbol]], resultTpe: Type): TypeEntity = paramss match {
      case Nil => createTypeEntity(resultTpe, Covariant)
      case params :: rest =>
        val paramTypes = params.map(p => createTypeEntity(p.tpe, Contravariant))
        val resultType = rec(rest, resultTpe.resultType)
        TypeEntity.methodInvocation(paramTypes, resultType)
    }

    (typeParams, rec(sym.paramss, sym.tpe.resultType))
  }

  private def createTypeParamEntity(typeSym: Symbol) =
    TypeParameterEntity(
      qualifiedName(typeSym),
      if (typeSym.variance.isCovariant)
        Covariant
      else if (typeSym.variance.isContravariant)
        Contravariant
      else Invariant,
      qualifiedName(typeSym.tpe.bounds.lo.typeSymbol),
      qualifiedName(typeSym.tpe.bounds.hi.typeSymbol))
}

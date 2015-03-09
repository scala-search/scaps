package scala.tools.apiSearch.featureExtraction

import scala.tools.nsc.interactive.Global
import scala.tools.apiSearch.model._

trait EntityFactory {
  val compiler: Global

  import compiler._

  def createClassEntity(sym: Symbol): ClassEntity = {
    ClassEntity(qualifiedName(sym), typeParamsFromOwningTemplates(sym), sym.baseClasses.tail.map(base => createTypeEntity(base.tpe, Covariant)))
  }

  def createTermEntity(sym: Symbol, rawComment: String): TermEntity = {
    val (typeParams, tpe) = createTypeEntity(sym)
    TermEntity(qualifiedName(sym), typeParams, tpe, rawComment)
  }

  def createTypeEntity(sym: Symbol): (List[TypeParameterEntity], TypeEntity) = {
    val (params, memberType) =
      if (sym.isMethod)
        methodType(sym)
      else
        (Nil, createTypeEntity(sym.tpe, Covariant))

    if (sym.owner.isClass && !sym.owner.isModuleClass) {
      (typeParamsFromOwningTemplates(sym) ++ params,
        TypeEntity("scala.Function1", Covariant, List(TypeEntity(qualifiedName(sym.owner), Contravariant, Nil), memberType)))
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
        val name = s"scala.Function${params.length}"
        val paramTypeArgs = params.map(p => createTypeEntity(p.tpe, Contravariant))
        val resultTypeArg = rec(rest, resultTpe.resultType)
        TypeEntity(name, Covariant, paramTypeArgs :+ resultTypeArg)
    }

    (typeParams, rec(sym.paramss, sym.tpe.resultType))
  }

  private def createTypeParamEntity(typeSym: Symbol) =
    TypeParameterEntity(
      qualifiedName(typeSym),
      qualifiedName(typeSym.tpe.bounds.lo.typeSymbol),
      qualifiedName(typeSym.tpe.bounds.hi.typeSymbol))
}

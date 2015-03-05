package scala.tools.apiSearch.featureExtraction

import scala.tools.nsc.interactive.Global
import scala.tools.apiSearch.model._

trait EntityFactory {
  val compiler: Global

  import compiler._

  def createTermEntity(sym: Symbol, rawComment: String): TermEntity = {
    val (typeParams, tpe) = createTypeEntity(sym)
    TermEntity(sym.fullName, typeParams, tpe, rawComment)
  }

  def createTypeEntity(sym: Symbol): (List[TypeParameterEntity], TypeEntity) = {
    val (params, memberType) =
      if (sym.isMethod)
        methodType(sym)
      else
        (Nil, createTypeEntity(sym.tpe, Covariant))

    if (sym.owner.isClass && !sym.owner.isModuleClass)
      (params, TypeEntity("scala.Function1", Covariant, List(TypeEntity(sym.owner.fullName, Contravariant, Nil), memberType)))
    else
      (params, memberType)
  }

  private def createTypeEntity(tpe: Type, variance: Variance): TypeEntity = {
    val name = tpe.typeSymbol.fullName

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
    TypeEntity(name.toString(), variance, args)
  }

  private def methodType(sym: Symbol): (List[TypeParameterEntity], TypeEntity) = {
    val typeParams = sym.tpe.typeParams.zipWithIndex.map {
      case (param, idx) => TypeParameterEntity(idx, "scala.Nothing", "scala.Any")
    }

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
}
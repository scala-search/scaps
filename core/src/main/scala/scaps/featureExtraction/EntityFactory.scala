package scaps.featureExtraction

import scaps.webapi._
import scala.tools.nsc.interactive.Global
import scala.util.Try
import scalaz.{ Contravariant => _, _ }
import scaps.utils.Logging
import scala.annotation.tailrec

trait EntityFactory extends Logging {
  val compiler: Global

  import compiler.{ TypeDef => _, TypeRef => _, _ }

  def extractEntities(classSym: Symbol, getDocComment: (Symbol, Symbol) => String): List[ExtractionError \/ Definition] =
    if (isTypeOfInterest(classSym)) {
      logger.trace(s"Extract entities in ${qualifiedName(classSym, true)}")

      val cls = createTypeDef(classSym)

      val objValue =
        if (isValueOfInterest(classSym)) Some(createValueDef(classSym, false, getDocComment(classSym, classSym)))
        else None

      val memberSymsWithComments = classSym.tpe.members
        .filter(isValueOfInterest)
        .map { m =>
          val copy = m.cloneSymbol(classSym)
          copy.info = classSym.tpe.memberInfo(m)
          (copy, m.isPrimaryConstructor, getDocComment(m, classSym))
        }

      val referencedTypeDefs = memberSymsWithComments.flatMap {
        case (sym, _, _) =>
          sym.tpe.collect { case t => t.typeSymbol }
            .filter(isTypeOfInterest _)
            .map(createTypeDef _)
      }.toList

      val members = memberSymsWithComments
        .map((createValueDef _).tupled)
        .toList

      cls :: objValue.toList ::: members ::: referencedTypeDefs
    } else {
      Nil
    }

  def createTypeDef(sym: Symbol): ExtractionError \/ TypeDef =
    \/.fromTryCatchNonFatal {
      val baseTypes = sym.tpe.baseTypeSeq.toList.tail
        .filter(tpe => isTypeOfInterest(tpe.typeSymbol))
        .map(tpe => createTypeRef(tpe, Covariant))
      TypeDef(qualifiedName(sym, true), typeParamsFromOwningTemplates(sym), baseTypes)
    }.leftMap(ExtractionError(qualifiedName(sym, true), _))

  def isTypeOfInterest(sym: Symbol): Boolean =
    (sym.isClass || sym.isModuleOrModuleClass) &&
      !sym.isAnonOrRefinementClass &&
      !sym.isLocalClass &&
      sym.isPublic

  def createValueDef(sym: Symbol, isPrimaryCtor: Boolean, rawComment: String): ExtractionError \/ ValueDef =
    \/.fromTryCatchNonFatal {
      val (typeParams, tpe) = createTypeRef(sym)

      val flags = Set.newBuilder[ValueDef.Flag]

      val isOverride =
        !sym.allOverriddenSymbols.isEmpty ||
          (sym.owner.isModule && sym.owner.baseClasses.drop(1).exists(_.tpe.decl(sym.name) != NoSymbol))

      val isImplicit =
        sym.isImplicit || (isPrimaryCtor && sym.owner.isImplicit)

      // reimplements Symbol#isStatic to work on inherited members
      def isStatic(s: Symbol): Boolean =
        s.owner.hasPackageFlag ||
          ((s.owner.hasModuleFlag || s.isConstructor) && isStatic(s.owner))

      if (isOverride) flags += ValueDef.Overrides
      if (isImplicit) flags += ValueDef.Implicit
      if (isStatic(sym)) flags += ValueDef.Static

      ValueDef(qualifiedName(sym, false), typeParams, tpe, rawComment, flags.result)
    }.leftMap(ExtractionError(qualifiedName(sym, false), _))

  def isValueOfInterest(sym: Symbol): Boolean =
    (sym.isValue || (sym.isConstructor && !sym.owner.isAbstractClass)) &&
      sym.isPublic &&
      !sym.isSynthetic

  def createTypeRef(sym: Symbol): (List[TypeParameterEntity], TypeRef) = {
    val (params, memberType) =
      if (sym.isMethod)
        methodType(sym)
      else if (sym.isModule)
        moduleType(sym)
      else
        (Nil, createTypeRef(sym.tpe, Covariant))

    if (sym.owner.isClass && !sym.owner.isModuleClass && !sym.isConstructor) {
      val ownerParams = typeParamsFromOwningTemplates(sym)
      val ownerArgs = ownerParams.map(p => TypeRef(p.name, Contravariant * p.variance, Nil))
      (ownerParams ++ params,
        TypeRef.MemberAccess(TypeRef(qualifiedName(sym.owner, true), Contravariant, ownerArgs), memberType))
    } else {
      (params, memberType)
    }
  }

  private def typeParamsFromOwningTemplates(sym: Symbol): List[TypeParameterEntity] = {
    sym.ownerChain.reverse.flatMap { owner =>
      owner.tpe.typeArgs.map(arg => createTypeParamEntity(arg.typeSymbol))
    }
  }

  def qualifiedName(sym: Symbol, isTypeName: Boolean): String = {
    def rec(s: Symbol): String = {
      val owner = s.owner

      if (s.isRootSymbol || s == s.owner)
        ""
      else if (s.isPackageObject)
        rec(owner)
      else
        EntityName.appendMember(rec(owner), s.decodedName)
    }

    val name =
      if (sym.isTypeParameter)
        sym.decodedName
      else
        rec(sym)

    if (isTypeName)
      s"$name${sym.moduleSuffix}"
    else
      name
  }

  private def createTypeRef(tpe: Type, variance: Variance): TypeRef = {
    def getVariance(idx: Int) = {
      val nscVariance =
        if (tpe.typeSymbol.isTypeParameter && tpe.bounds.hi.typeSymbol.typeParams.isDefinedAt(idx))
          tpe.bounds.hi.typeSymbol.typeParams(idx).variance
        else
          tpe.typeSymbol.typeParams(idx).variance
      if (nscVariance.isPositive)
        variance
      else if (nscVariance.isContravariant)
        variance.flip
      else
        Invariant
    }

    val args = tpe.typeArgs.zipWithIndex.map {
      case (arg, idx) => createTypeRef(arg, getVariance(idx))
    }

    if (tpe.typeSymbol.name == compiler.typeNames.BYNAME_PARAM_CLASS_NAME) {
      assert(args.length == 1)
      TypeRef.ByName(args.head, variance)
    } else if (tpe.typeSymbol.name == compiler.typeNames.REPEATED_PARAM_CLASS_NAME) {
      assert(args.length == 1)
      TypeRef.Repeated(args.head, variance)
    } else if (tpe.typeSymbol.name == compiler.typeNames.REFINE_CLASS_NAME) {
      val parents = tpe.parents.map(createTypeRef(_, variance))
      TypeRef.Refinement(parents, variance)
    } else {
      TypeRef(qualifiedName(tpe.typeSymbol, true), variance, args, tpe.typeSymbol.isTypeParameter)
    }
  }

  private def methodType(sym: Symbol): (List[TypeParameterEntity], TypeRef) = {
    val typeParams = sym.tpe.typeParams.map(createTypeParamEntity)

    def rec(paramss: List[List[Symbol]], resultTpe: Type): TypeRef = paramss match {
      case Nil => createTypeRef(resultTpe, Covariant)
      case params :: rest =>
        val paramTypes = params.map { p =>
          val pTpe = createTypeRef(p.tpe, Contravariant)
          if (p.isImplicit)
            TypeRef.Implicit(pTpe, Contravariant)
          else
            pTpe
        }
        val resultType = rec(rest, resultTpe.resultType)
        TypeRef.MethodInvocation(paramTypes, resultType)
    }

    (typeParams, rec(sym.paramss, sym.tpe.resultType))
  }

  private def moduleType(sym: Symbol): (List[TypeParameterEntity], TypeRef) = {
    val args = sym.tpe.parents.map { parent =>
      createTypeRef(parent, Covariant)
    }

    (Nil, TypeRef.Refinement(args))
  }

  private def createTypeParamEntity(typeSym: Symbol) =
    TypeParameterEntity(
      qualifiedName(typeSym, true),
      if (typeSym.variance.isCovariant)
        Covariant
      else if (typeSym.variance.isContravariant)
        Contravariant
      else Invariant,
      qualifiedName(typeSym.tpe.bounds.lo.typeSymbol, true),
      qualifiedName(typeSym.tpe.bounds.hi.typeSymbol, true))
}

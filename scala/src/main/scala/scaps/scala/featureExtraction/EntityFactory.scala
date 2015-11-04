package scaps.scala.featureExtraction

import scaps.api._
import scala.tools.nsc.Global
import scala.util.Try
import scalaz.{ Contravariant => _, _ }
import scalaz.std.boolean._
import scala.annotation.tailrec
import scala.tools.nsc.doc.model.ModelFactory
import scala.tools.nsc.doc.Settings
import scala.tools.nsc.doc.model.CommentFactory
import scala.tools.nsc.doc.model.TreeFactory
import scala.tools.nsc.doc.model.ModelFactoryImplicitSupport
import scala.tools.nsc.doc.model.ModelFactoryTypeSupport
import scala.tools.nsc.doc.model.MemberLookup
import scala.tools.nsc.doc.model.diagram.DiagramFactory
import scala.collection.immutable.SortedMap
import com.typesafe.scalalogging.StrictLogging
import scala.collection.parallel.immutable.ParRange
import scala.util.Failure

trait EntityFactory extends StrictLogging {
  val compiler: Global

  val scaladoc = new ScalaDocParser(compiler, compiler.settings.asInstanceOf[Settings])

  import compiler.{ TypeDef => _, TypeRef => _, DocComment => _, Try => _, _ }

  def extractEntities(classSym: Symbol): Seq[ExtractionError \/ Definition] =
    if (isTypeOfInterest(classSym)) {
      logger.trace(s"Extract entities in ${qualifiedName(classSym, true)}")

      val cls = withViews(classSym)(createTypeDef(classSym))

      val objValue = createValueWithViewDefs(classSym, false, getDocComment(classSym, classSym))

      val memberSymsWithComments = (classSym.tpe.members
        .filter(isValueOfInterest)
        .flatMap { m =>
          val copy = m.cloneSymbol(classSym)
          val comment = getDocComment(m, classSym)

          copy.info = classSym.tpe.memberInfo(m)

          val useCases = compiler.useCases(m, classSym).map {
            case (uc, _, _) =>
              val copy = uc.cloneSymbol(classSym)
              (copy, uc.isPrimaryConstructor, comment)
          }

          (copy, m.isPrimaryConstructor, comment) :: useCases
        }).toList

      val referencedTypeDefs = memberSymsWithComments.flatMap {
        case (sym, _, _) =>
          sym.tpe.collect { case t => t.typeSymbol }
            .flatMap(createTypeDef _)
      }.toList

      val members = memberSymsWithComments
        .flatMap((createValueWithViewDefs _).tupled)
        .toList

      cls ++ objValue ++ members ++ referencedTypeDefs
    } else {
      Nil
    }

  def getDocComment(sym: Symbol, site: Symbol) = {
    scaladoc(compiler.expandedDocComment(sym, site))
  }

  def createTypeDef(sym: Symbol): Seq[ExtractionError \/ Definition] =
    if (isTypeOfInterest(sym)) Seq(
      \/.fromTryCatchNonFatal {
        TypeDef(qualifiedName(sym, true), typeParamsFromOwningTemplates(sym))
      }.leftMap(ExtractionError(qualifiedName(sym, true), _)))
    else
      Seq()

  def isTypeOfInterest(sym: Symbol): Boolean =
    (sym.isClass || sym.isModuleOrModuleClass) &&
      !sym.isAnonOrRefinementClass &&
      !sym.isLocalClass &&
      sym.isPublic

  def createViewDef(sym: Symbol, fromDef: Definition): Seq[Definition] = {
    fromDef match {
      case c: TypeDef  => createViewFromTypeDef(c, sym)
      case t: ValueDef => createViewFromValue(t)
      case _           => List()
    }
  }

  def withViews(sym: Symbol)(res: Seq[ExtractionError \/ Definition]): Seq[ExtractionError \/ Definition] =
    res ++ res.flatMap {
      case \/-(definition) => createViewDef(sym, definition).map(\/.right)
      case _               => Seq()
    }

  def createValueWithViewDefs(sym: Symbol, isPrimaryCtor: Boolean, comment: DocComment): Seq[ExtractionError \/ Definition] =
    withViews(sym)(if (isValueOfInterest(sym)) Seq(
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

        ValueDef(qualifiedName(sym, false), typeParams, tpe, comment,
          flags = flags.result,
          docLink = link(sym))
      }.leftMap(ExtractionError(qualifiedName(sym, false), _)))
    else
      Seq())

  def link(sym: Symbol): Option[String] = {
    // Generates a ScalaDoc Link
    // Reuses the logic provided in https://github.com/scala/scala/blob/2.11.x/src/scaladoc/scala/tools/nsc/doc/base/MemberLookupBase.scala

    def linkName(sym: Symbol) = {
      def isRoot(s: Symbol) = (s eq NoSymbol) || s.isRootSymbol || s.isEmptyPackage || s.isEmptyPackageClass
      def nameString(s: Symbol) = s.nameString + (if ((s.isModule || s.isModuleClass) && !s.hasPackageFlag) "$" else "")
      val packageSuffix = if (sym.hasPackageFlag) ".package" else ""

      (sym.ownerChain.reverse.filterNot(isRoot(_)).map(nameString(_)).mkString(".") + packageSuffix)
        .replace("$.", "$$")
    }

    def externalSignature(sym: Symbol) = {
      sym.info // force it, otherwise we see lazy types
      (sym.nameString + sym.signatureString).replaceAll("\\s", "")
    }

    val owner = sym.owner

    if (sym.isClass || sym.isModule || sym.isTrait || sym.hasPackageFlag)
      Some(linkName(sym))
    else if (owner.isClass || owner.isModule || owner.isTrait || owner.hasPackageFlag)
      Some(linkName(owner) + "@" + externalSignature(sym))
    else
      None
  }

  def isValueOfInterest(sym: Symbol): Boolean =
    (sym.isValue || (sym.isConstructor && !sym.owner.isAbstractClass)) &&
      sym.isPublic

  def createTypeRef(sym: Symbol): (List[TypeParameter], TypeRef) = {
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

  private def typeParamsFromOwningTemplates(sym: Symbol): List[TypeParameter] = {
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

    val args =
      if (tpe.typeArgs.length != tpe.typeSymbol.typeParams.length)
        Nil
      else
        tpe.typeArgs.zipWithIndex.map {
          case (arg, idx) => createTypeRef(arg, getVariance(idx))
        }

    if (tpe.typeSymbol.name == compiler.typeNames.BYNAME_PARAM_CLASS_NAME) {
      assert(args.length == 1)
      TypeRef.ByName(args.head, variance)
    } else if (tpe.typeSymbol.name == compiler.typeNames.REFINE_CLASS_NAME) {
      val parents = tpe.parents.map(createTypeRef(_, variance))
      TypeRef.Refinement(parents, variance)
    } else {
      TypeRef(qualifiedName(tpe.typeSymbol, true), variance, args, tpe.typeSymbol.isTypeParameter)
    }
  }

  private def methodType(sym: Symbol): (List[TypeParameter], TypeRef) = {
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

  private def moduleType(sym: Symbol): (List[TypeParameter], TypeRef) = {
    val args = sym.tpe.parents.map { parent =>
      createTypeRef(parent, Covariant)
    }

    (Nil, TypeRef.Refinement(args))
  }

  private def createTypeParamEntity(typeSym: Symbol) =
    TypeParameter(
      qualifiedName(typeSym, true),
      if (typeSym.variance.isCovariant)
        Covariant
      else if (typeSym.variance.isContravariant)
        Contravariant
      else Invariant,
      qualifiedName(typeSym.tpe.bounds.lo.typeSymbol, true),
      qualifiedName(typeSym.tpe.bounds.hi.typeSymbol, true))

  private def createViewFromTypeDef(cls: TypeDef, clsSym: Symbol): List[ViewDef] = {
    val baseTypes = clsSym.tpe.baseTypeSeq.toList.tail
      .filter(tpe => isTypeOfInterest(tpe.typeSymbol))
      .map(tpe => createTypeRef(tpe, Covariant))

    val toRepeated = {
      // create implicit conversions from Seq and subtypes thereof to repeated args
      if (cls.name == TypeRef.Seq.name) {
        val p = cls.typeParameters(0)
        ViewDef.bidirectional(TypeRef.Repeated(TypeRef(p.name, Covariant, Nil, true)), cls.toType, implicitConversionDistance, "")
      } else {
        baseTypes.flatMap {
          case TypeRef.Seq(t, _) =>
            ViewDef.bidirectional(TypeRef.Repeated(t), cls.toType, implicitConversionDistance, "")
          case _ =>
            Nil
        }
      }
    }

    baseTypes.flatMap { baseCls =>
      ViewDef.bidirectional(baseCls, cls.toType, subtypingDistance, cls.name)
    } ++ toRepeated
  }

  private def createViewFromValue(v: ValueDef): List[ViewDef] = {
    def hasImplicitParam(t: TypeRef): Boolean =
      t.name == TypeRef.Implicit.name || t.args.exists(hasImplicitParam)

    if (v.isImplicit && v.isStatic && !hasImplicitParam(v.tpe)) {
      v.tpe.etaExpanded match {
        case TypeRef.Function(from :: Nil, to, _) if !from.isTypeParam =>
          ViewDef.bidirectional(from, to.withVariance(Contravariant), implicitConversionDistance, v.name)
        case _ =>
          Nil
      }
    } else
      Nil
  }

  val subtypingDistance = 0.5f
  val implicitConversionDistance = 0.9f
  val aliasDistance = 0.95f
  val identityDistance = 1f
}

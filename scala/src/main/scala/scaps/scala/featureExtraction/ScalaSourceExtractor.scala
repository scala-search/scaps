/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.scala.featureExtraction

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.doc.ScaladocGlobal
import scalaz.{ Contravariant => _, _ => _ }
import scaps.api._

class ScalaSourceExtractor(val compiler: ScaladocGlobal) extends EntityFactory {
  import compiler.{ TypeRef => _, _ }

  def apply(sources: List[SourceFile]): Stream[ExtractionError \/ Definition] = {
    val r = new Run()
    r.compileSources(sources)

    (Scala.builtinDefinitions.map(\/.right).toStream ++ r.units.toStream.flatMap { cu =>
      val classes = findClasses(cu.body)

      classes.flatMap { cls =>
        try {
          extractEntities(cls)
        } catch {
          case t: Throwable =>
            \/.left(ExtractionError(qualifiedName(cls, true), t)) :: Nil
        }
      }
    }).distinct
  }

  private def findClasses(tree: Tree): List[Symbol] =
    traverse(tree) {
      case impl: ImplDef =>
        (impl.symbol :: Nil, true)
      case _: ValOrDefDef =>
        (Nil, false)
      case _ =>
        (Nil, true)
    }

  private def traverse[T](tree: Tree)(collect: Tree => (List[T], Boolean)): List[T] = {
    val ts = new ListBuffer[T]

    val traverser = new Traverser {
      override def traverse(t: Tree) = {
        val (newTs, descend) = collect(t)
        ts ++= newTs
        if (descend) {
          super.traverse(t)
        }
      }
    }

    traverser(tree)

    ts.toList
  }
}

object Scala {
  val parametrizedTopAndBottomTypes =
    (1 to 22).flatMap { n =>
      val params = (1 to n).map(m => TypeParameter(s"X$m", Invariant)).toList
      List(
        TypeDef(TypeRef.Top.name(n), params),
        TypeDef(TypeRef.Bottom.name(n), params))
    }

  val builtinTypes =
    List(
      TypeDef(TypeRef.Nothing.name, Nil),
      TypeDef(TypeRef.Unknown.name, Nil),
      TypeDef(TypeRef.Repeated.name, List(TypeParameter("X", Covariant)))) ++
      parametrizedTopAndBottomTypes

  val builtinViews =
    List(
      // +_ %> +Nothing
      ViewDef(TypeRef("_", Covariant, Nil, isTypeParam = true), TypeRef.Nothing(), ""),
      // o_ %> o<unknown>
      ViewDef(TypeRef("_", Invariant, Nil, isTypeParam = true), TypeRef.Unknown(Invariant), ""),
      // +_ %> o<unknown>
      ViewDef(TypeRef("_", Covariant, Nil, isTypeParam = true), TypeRef.Unknown(Invariant), ""),
      // -_ %> o<unknown>
      ViewDef(TypeRef("_", Contravariant, Nil, isTypeParam = true), TypeRef.Unknown(Invariant), ""),
      // +_ %> o_
      ViewDef(TypeRef("_", Covariant, Nil, isTypeParam = true), TypeRef("_", Invariant, Nil, isTypeParam = true), ""),
      // +_ %> oNothing
      ViewDef(TypeRef("_", Covariant, Nil, isTypeParam = true), TypeRef.Nothing(Invariant), ""),
      // -_ %> o_
      ViewDef(TypeRef("_", Contravariant, Nil, isTypeParam = true), TypeRef("_", Invariant, Nil, isTypeParam = true), ""),
      // -_ %> oAny
      ViewDef(TypeRef("_", Contravariant, Nil, isTypeParam = true), TypeRef.Any(Invariant), ""))

  val builtinDefinitions: List[Definition] =
    builtinTypes ++ builtinViews
}

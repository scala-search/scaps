package scaps.scala.featureExtraction

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.doc.ScaladocGlobal
import scalaz._
import scaps.api._

class ScalaSourceExtractor(val compiler: ScaladocGlobal) extends EntityFactory {
  import compiler.{ TypeRef => _, _ }

  def apply(sources: List[SourceFile]): List[ExtractionError \/ Definition] = {
    val r = new Run()
    r.compileSources(sources)

    (Scala.builtinDefinitions.map(\/.right) ++ r.units.flatMap { cu =>
      val classes = findClasses(cu.body)

      classes.flatMap { cls =>
        try {
          extractEntities(cls)
        } catch {
          case t: Throwable =>
            \/.left(ExtractionError(qualifiedName(cls, true), t)) :: Nil
        }
      }
    })
      .distinct
      .flatMap {
        case right @ \/-(definition) => right :: createViewFromEntity(definition).map(\/.right)
        case left                    => Seq(left)
      }
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
  val builtinTypes =
    List(
      TypeDef(TypeRef.Nothing.name, Nil, Nil),
      TypeDef(TypeRef.Unknown.name, Nil, Nil))

  val builtinViews =
    List(
      // Nothing is a subtype of every type
      ViewDef(TypeRef("_", Covariant, Nil, isTypeParam = true), TypeRef.Nothing(), 1, ""),
      // Every invariant type is viewable as <unknown>
      ViewDef(TypeRef("_", Invariant, Nil, isTypeParam = true), TypeRef.Unknown(Invariant), 1, ""))

  val builtinDefinitions: List[Definition] =
    builtinTypes ++ builtinViews
}

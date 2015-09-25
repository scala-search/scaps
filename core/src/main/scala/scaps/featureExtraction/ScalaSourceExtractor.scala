package scaps.featureExtraction

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.doc.ScaladocGlobal

import scalaz.{ \/ => \/ }
import scaps.api.Definition

class ScalaSourceExtractor(val compiler: ScaladocGlobal) extends EntityFactory {
  import compiler._

  def apply(sources: List[SourceFile]): List[ExtractionError \/ Definition] = {
    val r = new Run()
    r.compileSources(sources)

    r.units.toList.flatMap { cu =>
      val classes = findClasses(cu.body)

      classes.flatMap { cls =>
        try {
          extractEntities(cls)
        } catch {
          case t: Throwable =>
            \/.left(ExtractionError(qualifiedName(cls, true), t)) :: Nil
        }
      }.distinct
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

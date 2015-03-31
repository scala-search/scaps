package scala.tools.apiSearch.featureExtraction

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.SourceFile
import scala.tools.apiSearch.model.Entity
import scala.tools.nsc.interactive.Global
import scala.util.Failure
import scala.util.Success

class ScalaSourceExtractor(val compiler: Global) extends EntityFactory {
  import compiler._

  def apply(sourceFile: SourceFile): Seq[Entity] =
    withTypedTree(sourceFile) { root =>
      compiler.ask { () =>
        val classes = findClasses(root)

        val fragments = for {
          sym <- symsMayContributingComments(root)
        } yield (sym, sourceFile)

        def getDocComment(sym: Symbol, site: Symbol): String = {
          val cr = new Response[(String, String, Position)]
          compiler.askDocComment(sym, sourceFile, site, fragments.toList, cr)
          cr.get.fold({ case (raw, _, _) => raw }, { case _ => "" })
        }

        classes.flatMap { cls =>
          scala.util.Try(extractEntities(cls, getDocComment _)).getOrElse(Nil)
        }
      }
    }.getOrElse(Nil).distinct

  private def findClasses(tree: Tree): List[Symbol] = {
    val classes = new ListBuffer[Symbol]

    val traverser = new Traverser {
      override def traverse(t: Tree) = {
        val descend = t match {
          case impl: ImplDef =>
            classes += impl.symbol
            true
          case _: ValOrDefDef =>
            false
          case _ =>
            true
        }
        if (descend) {
          super.traverse(t)
        }
      }
    }

    traverser(tree)

    classes.toList
  }

  private def symsMayContributingComments(tree: Tree): List[Symbol] = {
    val members = new ListBuffer[Symbol]

    val traverser = new Traverser {
      override def traverse(t: Tree) = {
        val descend = t match {
          case impl: ImplDef =>
            val sym = impl.symbol
            if (sym.isPublic) {
              members += sym

              members ++= sym.tpe.decls.filter(isTermOfInterest)
              true
            } else {
              false
            }
          case v: ValOrDefDef =>
            false
          case _ =>
            true
        }
        if (descend) {
          super.traverse(t)
        }
      }
    }

    traverser(tree)

    members.toList
  }

  private def withTypedTree[T](sourceFile: SourceFile)(f: Tree => T): util.Try[T] = {
    val r = new Response[Tree]
    compiler.askLoadedTyped(sourceFile, r)

    r.get.fold(Success(_), Failure(_)).map { root =>
      val result = f(root)

      compiler.removeUnitOf(sourceFile)

      result
    }
  }
}

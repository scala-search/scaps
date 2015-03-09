package scala.tools.apiSearch.featureExtraction

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.SourceFile
import scala.tools.apiSearch.model._
import scala.tools.nsc.interactive.Global

class ScalaSourceExtractor(val compiler: Global) extends EntityFactory {
  import compiler._

  def apply(sourceFile: SourceFile): Stream[Entity] = {
    val r = new Response[Tree]

    compiler.askLoadedTyped(sourceFile, r)

    val root = r.get.left.get

    compiler.ask { () =>
      val syms = rawEntities(root)

      val fragments = for { m <- syms } yield (m, sourceFile)

      syms.toStream.flatMap { sym =>
        compiler.ask { () =>
          if (sym.isClass) {
            scala.util.Try(createClassEntity(sym)).toOption
          } else {
            val cr = new Response[(String, String, Position)]
            compiler.askDocComment(sym, sourceFile, sym.enclosingPackage, fragments.toList, cr)
            val comment = cr.get.fold({ case (raw, _, _) => raw }, { case _ => "" })

            scala.util.Try(createTermEntity(sym, comment)).toOption
          }
        }
      }
    }
  }

  private def rawEntities(tree: Tree): List[Symbol] = {
    val members = new ListBuffer[Symbol]

    val traverser = new Traverser {
      override def traverse(t: Tree) = {
        val descend = t match {
          case impl: ImplDef =>
            val sym = impl.symbol
            if (sym.isPublic) {
              if (sym.isClass)
                members += sym

              members ++= sym.tpe.decls.filter(m => m.isTerm && m.isPublic && !m.isConstructor)
              true
            } else {
              false
            }
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

    members.toList
  }
}

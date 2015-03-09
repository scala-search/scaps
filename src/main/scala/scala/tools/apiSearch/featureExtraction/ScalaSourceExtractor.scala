package scala.tools.apiSearch.featureExtraction

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.SourceFile
import scala.tools.apiSearch.model._
import scala.tools.nsc.interactive.Global
import rx.lang.scala.Observable

class ScalaSourceExtractor(val compiler: Global) extends EntityFactory {
  import compiler._

  def apply(sourceFile: SourceFile): (Observable[TemplateEntity], Observable[TermEntity]) = {
    val r = new Response[Tree]

    compiler.askLoadedTyped(sourceFile, r)

    val root = r.get.left.get

    compiler.ask { () =>
      val ms = rawEntities(root)

      val fragments = for { m <- ms } yield (m, sourceFile)

      val (templates, members) = ms.partition(_.isClass)

      val memberEntities = Observable.from(members).flatMapIterable { member =>
        compiler.ask { () =>
          val cr = new Response[(String, String, Position)]
          compiler.askDocComment(member, sourceFile, member.enclosingPackage, fragments.toList, cr)
          val comment = cr.get.fold({ case (raw, _, _) => raw }, { case _ => "" })

          scala.util.Try(createTermEntity(member, comment)).toOption
        }
      }

      (Observable.empty, memberEntities)
    }
  }

  private def rawEntities(tree: Tree): List[Symbol] = {
    val members = new ListBuffer[Symbol]

    val traverser = new Traverser {
      override def traverse(t: Tree) = {
        val descend = t match {
          case impl: ImplDef =>
            val sym = impl.symbol
            if (sym.isClass)
              members += sym
            members ++= sym.tpe.decls.filter(m => m.isTerm && m.isPublic && !m.isConstructor)
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

    members.toList
  }
}

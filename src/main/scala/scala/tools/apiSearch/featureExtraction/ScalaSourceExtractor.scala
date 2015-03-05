package scala.tools.apiSearch.featureExtraction

import scala.tools.nsc.interactive.Global
import scala.reflect.internal.util.SourceFile
import scala.tools.apiSearch.model.TermEntity
import scala.collection.mutable.ListBuffer

class ScalaSourceExtractor(val compiler: Global) extends EntityFactory {
  import compiler._

  def apply(sourceFile: SourceFile): List[TermEntity] = {
    val r = new Response[Tree]

    compiler.askLoadedTyped(sourceFile, r)

    val root = r.get.left.get

    compiler.ask { () =>
      val ms = members(root)

      val fragments = for { m <- ms } yield (m, sourceFile)

      ms.map { member =>
        val cr = new Response[(String, String, Position)]
        compiler.askDocComment(member, sourceFile, member.enclosingPackage, fragments.toList, cr)
        val comment = cr.get.fold({ case (raw, _, _) => raw }, { case _ => "" })

        createTermEntity(member, comment)
      }
    }
  }

  private def findMembers(tree: Tree): List[MemberDef] = tree match {
    case PackageDef(pid, stmts) => stmts.flatMap(findMembers)
    case impl: ImplDef          => impl :: impl.impl.body.flatMap(findMembers)
    case m: MemberDef           => List(m)
    case _                      => Nil
  }

  private def members(tree: Tree): List[Symbol] = {
    val members = new ListBuffer[Symbol]

    val traverser = new Traverser {
      override def traverse(t: Tree) = {
        t match {
          case impl: ImplDef =>
            val sym = impl.symbol
            members += sym
            members ++= sym.tpe.members
          case _ => ()
        }
        super.traverse(t)
      }
    }
    
    traverser(tree)

    members.filter(_.isPublic).toList
  }
}
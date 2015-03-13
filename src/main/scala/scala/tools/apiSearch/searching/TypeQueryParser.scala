package scala.tools.apiSearch.queryParser

import scala.tools.nsc.interactive.Global
import scala.tools.apiSearch.model._
import scala.reflect.internal.util.BatchSourceFile
import scala.collection.mutable.ListBuffer
import scala.tools.apiSearch.featureExtraction.EntityFactory

case class TypeQuery(parts: (Variance, ClassEntity)*)

class TypeQueryParser(val compiler: Global) extends EntityFactory {
  import compiler._

  def apply(query: String): Seq[(Variance, ClassEntity)] = {
    val source = new BatchSourceFile("xyz", s"""
      package scala.tools.apiSearch.searching

      trait T {
        def x: $query
      }""")

    val r = new Response[Tree]

    compiler.askLoadedTyped(source, r)

    val sym = getTypeSymbol(r.get.left.get)

    (Covariant, createClassEntity(sym)) :: Nil
  }

  private def getTypeSymbol(t: Tree): Symbol = {
    var result = ListBuffer[Symbol]()

    val traverser = new Traverser {
      override def traverse(t: Tree) = {
        val descend = t match {
          case d: DefDef =>
            result += d.symbol.tpe.typeSymbol
            false
          case _ =>
            true
        }
        if (descend) {
          super.traverse(t)
        }
      }
    }
    traverser(t)

    result.head
  }
}

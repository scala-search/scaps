package scala.tools.apiSearch.queryParser

import scala.tools.apiSearch.model._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.tools.apiSearch.utils.CompilerAccess
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.interactive.Response

class TypeQueryParserSpecs extends FlatSpec with Matchers with CompilerAccess {
  "the type query parser" should "parse types" in {
    withQueryParser {
      """
      package scala.tools.apiSearch.searching

      class C
      """
    } { parser =>
      val query = parser("p.C")
      query should contain((Covariant, ClassEntity("p.C", Nil, List(TypeEntity.anyRef, TypeEntity.any))))
    }
  }

  def withQueryParser(sources: String*)(f: TypeQueryParser => Unit): Unit = {
    val r = new Response[Unit]
    val sourceFiles = sources.toList.map(source => new BatchSourceFile("", source))
    compiler.askReload(sourceFiles, r)
    r.get

    f(new TypeQueryParser(compiler))

    compiler.askFilesDeleted(sourceFiles, new Response[Unit])
  }
}

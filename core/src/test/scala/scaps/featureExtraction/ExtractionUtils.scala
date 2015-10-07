package scaps.featureExtraction

import scaps.api._
import scaps.scala.featureExtraction.CompilerUtils;
import scaps.scala.featureExtraction.ExtractionError;
import scaps.scala.featureExtraction.ScalaSourceExtractor;
import scala.util.Random
import scala.reflect.internal.util.BatchSourceFile
import scalaz.std.list._
import scalaz.std.stream._

import org.scalatest.Matchers

trait ExtractionUtils extends Matchers {

  def extractAll(sources: String*): Seq[Definition] = {
    val files = sources.toList.zipWithIndex.map {
      case (src, idx) =>
        new BatchSourceFile(s"f$idx.scala", src)
    }

    val compiler = CompilerUtils.createCompiler(Nil)

    val extractor = new ScalaSourceExtractor(compiler)

    ExtractionError.handleErrors(extractor(files)) {
      e => fail(e.entityName, e.error)
    }
  }

  def extractAllValues(source: String): Seq[ValueDef] = {
    extractAll(source).collect { case t: ValueDef => t }
  }

  def extractValues(source: String)(entityHandlers: (String, ValueDef => Unit)*): Unit = {
    val entities = extractAllValues(source)
    val names = entities.map(_.name)

    entityHandlers.foreach { handler =>
      names should contain(handler._1)
      withClue(s"number of entities with name ${handler._1}") {
        names.count(_ == handler._1) should be(1)
      }
      entities.find(_.name == handler._1).fold(throw new Exception)(handler._2)
    }
  }

  def shouldExtractValues(source: String)(entityNames: String*): Unit =
    extractValues(source)(entityNames.map(n => (n, (_: ValueDef) => ())): _*)

  def extractAllTypeDefs(source: String): Seq[TypeDef] = {
    extractAll(source).collect { case c: TypeDef => c }
  }

  def extractTypeDefs(source: String)(entityHandlers: (String, TypeDef => Unit)*): Unit = {
    val entities = extractAllTypeDefs(source)
    val names = entities.map(_.name)

    entityHandlers.foreach { handler =>
      names should contain(handler._1)
      withClue(s"number of entities with name ${handler._1}") {
        names.count(_ == handler._1) should be(1)
      }
      entities.find(_.name == handler._1).fold(throw new Exception)(handler._2)
    }
  }

  def extractAllViews(source: String): Seq[ViewDef] = {
    extractAll(source).collect { case v: ViewDef => v }
  }
}

package scaps.featureExtraction

import scaps.api._
import scala.util.Random
import scala.reflect.internal.util.BatchSourceFile
import scalaz.std.list._

import org.scalatest.Matchers

trait ExtractionUtils extends Matchers {

  def extractAll(source: String): Seq[Definition] =
    CompilerUtils.withCompiler() { compiler =>
      val extractor = new ScalaSourceExtractor(compiler)
      val randomFileName = s"${Random.nextInt()}.scala"

      ExtractionError.handleErrors(extractor(new BatchSourceFile(randomFileName, source))) {
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
}

package scaps.featureExtraction

import scaps.webapi._
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

  def extractAllTerms(source: String): Seq[TermEntity] = {
    extractAll(source).collect { case t: TermEntity => t }
  }

  def extractTerms(source: String)(entityHandlers: (String, TermEntity => Unit)*): Unit = {
    val entities = extractAllTerms(source)
    val names = entities.map(_.name)

    entityHandlers.foreach { handler =>
      names should contain(handler._1)
      withClue(s"number of entities with name ${handler._1}") {
        names.count(_ == handler._1) should be(1)
      }
      entities.find(_.name == handler._1).fold(throw new Exception)(handler._2)
    }
  }

  def shouldExtractTerms(source: String)(entityNames: String*): Unit =
    extractTerms(source)(entityNames.map(n => (n, (_: TermEntity) => ())): _*)

  def extractAllClasses(source: String): Seq[ClassEntity] = {
    extractAll(source).collect { case c: ClassEntity => c }
  }

  def extractClasses(source: String)(entityHandlers: (String, ClassEntity => Unit)*): Unit = {
    val entities = extractAllClasses(source)
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

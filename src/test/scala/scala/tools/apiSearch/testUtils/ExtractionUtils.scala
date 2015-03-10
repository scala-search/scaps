package scala.tools.apiSearch.testUtils

import scala.tools.apiSearch.utils.CompilerAccess
import scala.tools.apiSearch.featureExtraction.ScalaSourceExtractor
import scala.tools.apiSearch.model._
import scala.util.Random
import scala.reflect.internal.util.BatchSourceFile
import org.scalatest.Matchers

trait ExtractionUtils extends CompilerAccess {
  self: Matchers =>

  val extractor = new ScalaSourceExtractor(compiler)

  def extractAllTerms(source: String): Stream[TermEntity] = {
    val randomFileName = s"${Random.nextInt()}.scala"
    extractor(new BatchSourceFile(randomFileName, source)).collect { case t: TermEntity => t }
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

  def extractAllClasses(source: String): Stream[ClassEntity] = {
    val randomFileName = s"${Random.nextInt()}.scala"
    extractor(new BatchSourceFile(randomFileName, source)).collect { case c: ClassEntity => c }
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

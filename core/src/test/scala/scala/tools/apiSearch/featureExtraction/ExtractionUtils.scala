package scala.tools.apiSearch.featureExtraction

import scala.tools.apiSearch.model._
import scala.util.Random
import scala.reflect.internal.util.BatchSourceFile
import org.scalatest.Matchers

trait ExtractionUtils extends Matchers {
  val compiler = CompilerUtils.initCompiler()
  val extractor = new ScalaSourceExtractor(compiler)

  def extractAll(source: String): Seq[Entity] = {
    val randomFileName = s"${Random.nextInt()}.scala"
    extractor(new BatchSourceFile(randomFileName, source)).distinct
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

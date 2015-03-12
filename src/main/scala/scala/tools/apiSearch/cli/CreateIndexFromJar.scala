package scala.tools.apiSearch.cli

import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.tools.apiSearch.utils.CompilerAccess
import org.apache.lucene.store.Directory
import org.apache.lucene.store.FSDirectory
import java.nio.file.Path
import java.io.File
import java.nio.file.Paths
import scala.tools.apiSearch.index.TermsIndex
import scala.tools.apiSearch.index.ClassIndex
import scala.tools.apiSearch.model._

object CreateIndexFromJar extends App with CompilerAccess {
  val libraryPath = args(0)
  val indexDir = args(1)

  val extractor = new JarExtractor(compiler)

  val termsDir = FSDirectory.open(Paths.get(indexDir, "terms").toFile())
  val termsIndex = new TermsIndex(termsDir)

  val classesDir = FSDirectory.open(Paths.get(indexDir, "classes").toFile())
  val classesIndex = new ClassIndex(classesDir)

  termsIndex.delete()
  classesIndex.delete()

  val entities = extractor(new File(libraryPath))
  termsIndex.addEntities(entities.collect { case t: TermEntity => t })
  classesIndex.addEntities(entities.collect { case c: ClassEntity => c })
}

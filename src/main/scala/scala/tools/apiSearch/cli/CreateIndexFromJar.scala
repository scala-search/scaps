package scala.tools.apiSearch.cli

import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.tools.apiSearch.utils.CompilerAccess
import org.apache.lucene.store.Directory
import org.apache.lucene.store.FSDirectory
import java.nio.file.Path
import java.io.File
import java.nio.file.Paths
import scala.tools.apiSearch.index.TermsIndex
import scala.tools.apiSearch.model.TermEntity

object CreateIndexFromJar extends App with CompilerAccess {
  val libraryPath = args(0)
  val indexDir = args(1)

  val extractor = new JarExtractor(compiler)

  val dir = FSDirectory.open(Paths.get(indexDir, "terms").toFile())
  val index = new TermsIndex(dir)

  index.delete()

  val entities = extractor(new File(libraryPath))
  index.addEntities(entities.collect { case t: TermEntity => t })
}

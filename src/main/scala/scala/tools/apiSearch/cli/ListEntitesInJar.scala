package scala.tools.apiSearch.cli

import scala.tools.apiSearch.utils.CompilerAccess
import scala.tools.apiSearch.featureExtraction.JarExtractor

object ListEntitesInJar extends App with CompilerAccess{
  val extractor = new JarExtractor(compiler)
  val path = args(0)
  
  extractor(path).zipWithIndex.foreach(println)
}
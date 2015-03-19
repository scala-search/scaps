package scala.tools.apiSearch.cli

import org.apache.lucene.store.FSDirectory
import java.nio.file.Paths
import scala.tools.apiSearch.index.TermsIndex
import scala.tools.apiSearch.index.ClassIndex
import scala.tools.apiSearch.searching.QueryParser
import scala.io.StdIn
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.io.Source

object Search extends App {
  val indexDir = args(0)

  val termsDir = FSDirectory.open(Paths.get(indexDir, "terms").toFile())
  val termsIndex = new TermsIndex(termsDir)

  val classesDir = FSDirectory.open(Paths.get(indexDir, "classes").toFile())
  val classesIndex = new ClassIndex(classesDir)

  val analyzer = new QueryAnalyzer(classesIndex.findClass _)

  Source.stdin.getLines().takeWhile(_.nonEmpty).foreach { in =>
    QueryParser(in).right.foreach { raw =>
      println(raw)
      val res = analyzer(raw).get
      println(res)
      res.right.foreach { query =>
        println(termsIndex.find(query).get.take(10))
      }
    }
  }
}

package scala.tools.apiSearch.cli

import org.apache.lucene.store.FSDirectory
import java.nio.file.Paths
import scala.tools.apiSearch.index.TermsIndex
import scala.tools.apiSearch.index.ClassIndex
import scala.tools.apiSearch.searching.QueryParser
import scala.io.StdIn
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.tools.apiSearch.searching.Suggestion
import scala.io.Source

object Search extends App {
  val indexDir = args(0)

  val termsDir = FSDirectory.open(Paths.get(indexDir, "terms").toFile())
  val termsIndex = new TermsIndex(termsDir)

  val classesDir = FSDirectory.open(Paths.get(indexDir, "classes").toFile())
  val classesIndex = new ClassIndex(classesDir)

  val analyzer = new QueryAnalyzer(classesIndex.findClass _, classesIndex.findSubClasses _)

  Source.stdin.getLines().takeWhile(_.nonEmpty).foreach { in =>
    QueryParser(in).right.foreach { raw =>
      analyzer(raw).get.fold({
        case Suggestion(raw, Seq()) =>
          println(s"Type ${raw.tpe} not found")
        case Suggestion(raw, candidates) =>
          println(s"Type ${raw.tpe} is ambiguous")
          candidates.foreach(c => println(s"    ${c.name}"))
      }, {
        query =>
          termsIndex.find(query).get.take(10).foreach { t =>
            println(t)
          }
      })
    }
  }
}

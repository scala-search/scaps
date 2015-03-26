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
import scala.tools.apiSearch.index.Indexer

object Search extends App {
  val indexDir = args(0)

  val indexer = new Indexer(indexDir)

  val analyzer = QueryAnalyzer(indexer.classesIndex)

  Source.stdin.getLines().takeWhile(_.nonEmpty).foreach { in =>
    QueryParser(in).right.foreach { raw =>
      analyzer(raw).get.fold({
        case Suggestion(raw, Seq()) =>
          println(s"Type ${raw.name} not found")
        case Suggestion(raw, candidates) =>
          println(s"Type ${raw.name} is ambiguous")
          candidates.foreach(c => println(s"    ${c.name}"))
      }, {
        query =>
          println(query)
          indexer.termsIndex.find(query).get.take(10).foreach { t =>
            println(t.copy(comment = ""))
            println(t.fingerprint)
            println()
          }
      })
    }
  }
}

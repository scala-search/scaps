package scala.tools.apiSearch.cli

import org.apache.lucene.store.FSDirectory
import java.nio.file.Paths
import scala.tools.apiSearch.index.TermsIndex
import scala.tools.apiSearch.index.ClassIndex
import scala.tools.apiSearch.searching.QueryParser
import scala.io.StdIn
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.io.Source
import scala.tools.apiSearch.settings.QuerySettings
import scala.tools.apiSearch.index.Indexer
import scala.tools.apiSearch.searching.QueryAnalyzer._

object Search extends App {
  val indexDir = args(0)

  val indexer = new Indexer(indexDir)

  val analyzer = QueryAnalyzer(QuerySettings.fromApplicationConf(), indexer.classesIndex)

  Source.stdin.getLines().takeWhile(_.nonEmpty).foreach { in =>
    QueryParser(in).fold(println, { raw =>
      analyzer(raw).get.fold(errors => errors.foreach {
        case NameNotFound(raw) =>
          println(s"Type ${raw.name} not found")
        case NameAmbiguous(raw, candidates) =>
          println(s"Type ${raw.name} is ambiguous")
          candidates.foreach(c => println(s"    ${c.name}"))
        case IllegalNumberOfTypeArgs(raw, n) =>
          println(s"$raw has wrong number of arguments ($n expected)")
      }, {
        query =>
          println(query)
          indexer.termsIndex.find(query).get.take(10).foreach { t =>
            println(t.copy(comment = ""))
            println(t.fingerprint)
            println()
          }
      })
    })
  }
}

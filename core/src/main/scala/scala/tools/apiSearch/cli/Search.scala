package scala.tools.apiSearch.cli

import scala.io.Source
import scala.tools.apiSearch.index.Indexer
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.tools.apiSearch.searching.QueryAnalyzer.IllegalNumberOfTypeArgs
import scala.tools.apiSearch.searching.QueryAnalyzer.NameAmbiguous
import scala.tools.apiSearch.searching.QueryAnalyzer.NameNotFound
import scala.tools.apiSearch.searching.QueryParser
import scala.tools.apiSearch.settings.Settings

object Search extends App {
  val settings = Settings.fromApplicationConf()

  val indexer = new Indexer(settings)

  val analyzer = QueryAnalyzer(settings.query, indexer.classesIndex)

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

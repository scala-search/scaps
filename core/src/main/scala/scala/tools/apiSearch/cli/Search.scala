package scala.tools.apiSearch.cli

import scala.io.Source
import scala.tools.apiSearch.searchEngine.SearchEngine
import scala.tools.apiSearch.searchEngine.queries.QueryAnalyzer
import scala.tools.apiSearch.searchEngine.queries.QueryAnalyzer._
import scala.tools.apiSearch.searchEngine.queries.QueryParser
import scala.tools.apiSearch.settings.Settings

object Search extends App {
  val engine = SearchEngine(Settings.fromApplicationConf).get

  Source.stdin.getLines().takeWhile(_.nonEmpty).foreach { query =>
    engine.search(query).get.fold(
      errors => errors.foreach {
        case SyntaxError(msg) =>
          println(msg)
        case NameNotFound(raw) =>
          println(s"Type ${raw.name} not found")
        case NameAmbiguous(raw, candidates) =>
          println(s"Type ${raw.name} is ambiguous")
          candidates.foreach(c => println(s"    ${c.name}"))
        case IllegalNumberOfTypeArgs(raw, n) =>
          println(s"$raw has wrong number of arguments ($n expected)")
      },
      results => results.take(10).foreach { term =>
        println(term.withoutComment)
        println(term.fingerprint)
        println()
      })
  }
}

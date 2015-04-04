package scala.tools.apiSearch.cli

import scala.io.Source
import scala.tools.apiSearch.searchEngine.SearchEngine
import scala.tools.apiSearch.searchEngine.queries.QueryAnalyzer
import scala.tools.apiSearch.searchEngine.queries.QueryParser
import scala.tools.apiSearch.settings.Settings
import scala.tools.apiSearch.searchEngine.SyntaxError
import scala.tools.apiSearch.searchEngine.NameNotFound
import scala.tools.apiSearch.searchEngine.NameAmbiguous
import scala.tools.apiSearch.searchEngine.UnexpectedNumberOfTypeArgs

object Search extends App {
  val engine = SearchEngine(Settings.fromApplicationConf).get

  Source.stdin.getLines().takeWhile(_.nonEmpty).foreach { query =>
    engine.search(query).get.fold(
      errors => errors.foreach {
        case SyntaxError(msg) =>
          println(msg)
        case NameNotFound(name) =>
          println(s"Type ${name} not found")
        case NameAmbiguous(name, candidates) =>
          println(s"Type ${name} is ambiguous")
          candidates.foreach(c => println(s"    ${c.name}"))
        case UnexpectedNumberOfTypeArgs(raw, n) =>
          println(s"$raw has wrong number of arguments ($n expected)")
      },
      results => results.take(10).foreach { term =>
        println(term.withoutComment)
        println(term.fingerprint)
        println()
      })
  }
}

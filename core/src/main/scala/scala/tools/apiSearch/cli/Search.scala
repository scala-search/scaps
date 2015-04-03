package scala.tools.apiSearch.cli

import scala.io.Source
import scala.tools.apiSearch.SearchEngine
import scala.tools.apiSearch.searching.QueryAnalyzer
import scala.tools.apiSearch.searching.QueryAnalyzer.IllegalNumberOfTypeArgs
import scala.tools.apiSearch.searching.QueryAnalyzer.NameAmbiguous
import scala.tools.apiSearch.searching.QueryAnalyzer.NameNotFound
import scala.tools.apiSearch.searching.QueryParser
import scala.tools.apiSearch.settings.Settings
import scala.tools.apiSearch.searching.QueryAnalyzer.SyntaxError

object Search extends App {
  val engine = new SearchEngine(Settings.fromApplicationConf)

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

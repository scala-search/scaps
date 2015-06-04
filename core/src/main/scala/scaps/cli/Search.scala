package scaps.cli

import scala.io.Source
import scaps.searchEngine.NameAmbiguous
import scaps.searchEngine.NameNotFound
import scaps.searchEngine.SearchEngine
import scaps.searchEngine.SyntaxError
import scaps.searchEngine.TooUnspecific
import scaps.searchEngine.UnexpectedNumberOfTypeArgs
import scaps.settings.Settings

object Search extends App {
  val engine = SearchEngine(Settings.fromApplicationConf).get

  Source.stdin.getLines().takeWhile(_.nonEmpty).foreach { query =>
    engine.search(query).get.fold(
      error => error match {
        case SyntaxError(msg) =>
          println(msg)
        case NameNotFound(name) =>
          println(s"Type ${name} not found")
        case NameAmbiguous(name, candidates) =>
          println(s"Type ${name} is ambiguous")
          candidates.foreach(c => println(s"    ${c.name}"))
        case UnexpectedNumberOfTypeArgs(raw, n) =>
          println(s"$raw has wrong number of arguments ($n expected)")
        case TooUnspecific() =>
          println(s"Query too unspecific consider using wildcards '_' instead of 'Any' types")
      },
      results => results.take(10).foreach { term =>
        println(term.signature)
        println(term.tpe.normalize(term.typeParameters))
        println(term.typeFingerprint)
        println()
      })
  }
}

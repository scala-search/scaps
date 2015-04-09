package scala.tools.apiSearch.searchEngine

import scala.tools.apiSearch.model.ClassEntity

sealed trait QueryError

case class SyntaxError(msg: String) extends QueryError

sealed trait SemanticError extends QueryError
case class NameNotFound(name: String) extends SemanticError
case class NameAmbiguous(name: String, candidates: Seq[ClassEntity]) extends SemanticError
case class UnexpectedNumberOfTypeArgs(name: String, expectedArgs: Int) extends SemanticError

sealed trait ProcessingError extends QueryError
case class TooUnspecific() extends ProcessingError

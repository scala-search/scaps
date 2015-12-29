package scaps.searchEngine

import scaps.api.TypeDef

sealed trait QueryError

case class SyntaxError(msg: String) extends QueryError

sealed trait SemanticError extends QueryError
case class NameNotFound(name: String) extends SemanticError
case class NameAmbiguous(name: String, candidates: Seq[TypeDef]) extends SemanticError
case class UnexpectedNumberOfTypeArgs(name: String, expectedArgs: Int) extends SemanticError

sealed trait ProcessingError extends QueryError
case object TooUnspecific extends ProcessingError

case object MaximumClauseCountExceededException extends Exception

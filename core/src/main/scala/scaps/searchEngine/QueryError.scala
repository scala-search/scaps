/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine

import scaps.api.TypeDef

sealed trait QueryError

case class SyntaxError(msg: String) extends QueryError

sealed trait SemanticError extends QueryError
case class NameNotFound(name: String) extends SemanticError
case class NameAmbiguous(name: String, candidates: Seq[TypeDef]) extends SemanticError
case class UnexpectedNumberOfTypeArgs(name: String, expectedArgs: Int) extends SemanticError
case object MaximumClauseCountExceeded extends SemanticError

sealed trait ProcessingError extends QueryError
case object TooUnspecific extends ProcessingError

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.webservice.actors

import scalaz.\/
import scaps.api.ValueDef
import scaps.api.Module
import scaps.searchEngine.SearchEngine
import scaps.api.Definition
import scaps.api.Result

object ActorProtocol {
  case class Index(indexName: String, definitions: Seq[Definition])

  case class FinalizeIndex(indexName: String)
  case class Finalized(indexName: String)

  case class Search(query: String, moduleIds: Set[String], noResults: Int, offset: Int)
  type ResultSet = String \/ Seq[Result[ValueDef]]

  case class PositiveAssessement(query: String, moduleIds: Set[String], signature: String)

  case object GetStatus
}

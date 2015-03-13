package scala.tools.apiSearch.searching

import scala.tools.apiSearch.model._
import scala.tools.apiSearch.index.ClassIndex

case class Query(parts: List[(Variance, ClassEntity)])

/**
 *
 */
class QueryAnalyzer(classes: ClassIndex) {

}

package scaps.searchEngine.queries

import scalaz.{ \/ => \/ }
import scaps.api.Invariant
import scaps.api.TypeDef
import scaps.api.TypeRef
import scaps.api.ValueDef
import scaps.api.ViewDef
import scaps.searchEngine.ApiQuery
import scaps.searchEngine.ApiTypeQuery
import scaps.searchEngine.SemanticError
import scaps.settings.QuerySettings
import scaps.searchEngine.ApiQuery
import scaps.searchEngine.NameNotFound
import scaps.searchEngine.QueryError
import scaps.settings.Settings
import scala.util.matching.Regex
import java.util.regex.Pattern
import scaps.api.FingerprintTerm

/**
 * Analyzes queries from various representations.
 *
 * Instances require access to the class and view index which is injected via
 * `findTypeDefsBySuffix` and `findViews`
 */
class QueryAnalyzer private[searchEngine] (
    settings: Settings,
    findTypeDefsBySuffix: (String) => Seq[TypeDef],
    getTypeFrequency: FingerprintTerm => Double,
    findViews: (TypeRef) => Seq[ViewDef]) {

  val resolver = new QueryResolver(findTypeDefsBySuffix)
  val expander = new QueryExpander(settings.query, getTypeFrequency, findViews)

  def apply(query: String): QueryError \/ ApiQuery = {
    QueryParser(query).flatMap { parsed =>
      apply(parsed).swapped(_.flatMap {
        case e: NameNotFound =>
          parsed match {
            case RawQuery.Full("", tpe) if tpe.args.length == 0 =>
              apply(RawQuery.Keywords(tpe.name)).swap
            case _ =>
              \/.right(e)
          }
        case e =>
          \/.right(e)
      })
    }
  }

  /**
   * Transforms a parsed query into a query that can be passed to the values index.
   */
  def apply(raw: RawQuery): SemanticError \/ ApiQuery =
    raw match {
      case RawQuery.Keywords(keys) =>
        \/.right(ApiQuery(keys, None))
      case RawQuery.Full(keys, tpe) =>
        for {
          resolved <- resolver(tpe)
          normalized = resolved.normalize(Nil)
          typeQuery <- apply(normalized)
        } yield {
          ApiQuery(keys, Some(typeQuery))
        }
    }

  def apply(v: ValueDef): SemanticError \/ ApiTypeQuery =
    apply(v.tpe.normalize(v.typeParameters))

  def apply(t: TypeRef): SemanticError \/ ApiTypeQuery = {
    val polarized = if (settings.index.polarizedTypes) t else t.withVariance(Invariant)
    expander(polarized)
  }

  def favorTypesMatching(pattern: Pattern): QueryAnalyzer = {
    val findTypeDefs = findTypeDefsBySuffix andThen { candidates =>
      candidates.filter(t => pattern.matcher(t.name).matches) match {
        case favs if !favs.isEmpty => favs
        case _                     => candidates
      }
    }
    new QueryAnalyzer(settings, findTypeDefs, getTypeFrequency, findViews)
  }
}

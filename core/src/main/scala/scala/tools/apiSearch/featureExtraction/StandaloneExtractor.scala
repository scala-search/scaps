package scala.tools.apiSearch.featureExtraction

import scala.tools.apiSearch.settings.ExtractorSettings
import scala.tools.apiSearch.utils.CompilerAccess
import scala.tools.apiSearch.model.Entity

object StandaloneExtractor extends CompilerAccess {
  def apply(settings: ExtractorSettings): Stream[Entity] = {
    val extractor = new JarExtractor(compiler)

    settings.jars.toStream.flatMap { jar =>
      extractor.apply(jar)
    }
  }
}

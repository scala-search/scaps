package scala.tools.apiSearch.featureExtraction

import scala.tools.apiSearch.settings.ExtractorSettings
import scala.tools.apiSearch.utils.CompilerAccess
import scala.tools.apiSearch.model.Entity

class StandaloneExtractor(settings: ExtractorSettings) extends CompilerAccess {
  def apply(): Stream[Entity] = {
    val extractor = new JarExtractor(compiler)

    settings.jars.toStream.flatMap { jar =>
      extractor.apply(jar)
    }
  }
}

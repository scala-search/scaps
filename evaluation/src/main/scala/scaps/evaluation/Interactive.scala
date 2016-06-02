package scaps.evaluation

import scaps.settings.Settings
import scaps.searchEngine.SearchEngine
import java.io.File
import scaps.scala.featureExtraction.JarExtractor
import scaps.scala.featureExtraction.CompilerUtils
import scaps.scala.featureExtraction.ExtractionError
import scaps.utils.Logging
import scalaz.std.stream._
import scaps.searchEngine.queries.QueryAnalyzer
import scalaz.Memo
import java.util.regex.Pattern

object Interactive extends Logging {
  def extract(settings: Settings = Settings.fromApplicationConf, evaluationSettings: EvaluationSettings = EvaluationSettings.fromApplicationConf) = {
    val engine = SearchEngine(settings).get
    evaluationSettings.downloadDir.mkdirs()

    val classPaths = for {
      project <- evaluationSettings.projects
      dependency <- project.dependencies
    } yield {
      val file = new File(evaluationSettings.downloadDir, dependency.name)

      if (!file.exists()) {
        import sys.process._
        (dependency.url #> file).!!
      }

      file.getAbsolutePath()
    }

    val compiler = CompilerUtils.createCompiler(classPaths)
    val extractor = new JarExtractor(compiler)

    engine.resetIndexes().get

    evaluationSettings.projects.foreach { project =>
      val jar = new File(evaluationSettings.downloadDir, project.name)

      if (!jar.exists()) {
        import sys.process._
        (project.url #> jar).!!
      }

      def defs = ExtractionError.logErrors(extractor(jar), logger.info(_))

      engine.index(defs).get
    }
  }

  def updateStats(settings: Settings = Settings.fromApplicationConf) = {
    val engine = SearchEngine(settings).get
    engine.finalizeIndex().get
  }

  def analyze(query: String, settings: Settings = Settings.fromApplicationConf) = {
    val engine = SearchEngine(settings).get

    def findClassBySuffix(suffix: String) =
      engine.typeIndex.findTypeDefsBySuffix(suffix, Set()).get

    val analyzer = new QueryAnalyzer(
      settings,
      Memo.mutableHashMapMemo((findClassBySuffix _)),
      Memo.mutableHashMapMemo(engine.typeIndex.termFrequency(_, Set()).get),
      Memo.mutableHashMapMemo(engine.viewIndex.findAlternativesWithRetainedInfo(_, 0, Set()).get))
      .favorTypesMatching(Pattern.compile("""scala\..*"""))
      .favorTypesMatching(Pattern.compile("""(scala\.([^\.#]+))|java\.lang\.String"""))

    analyzer.apply(query)
  }
}

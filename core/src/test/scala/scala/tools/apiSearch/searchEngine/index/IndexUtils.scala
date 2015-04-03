package scala.tools.apiSearch.searchEngine.index

import scala.tools.apiSearch.featureExtraction.ExtractionUtils
import scala.tools.apiSearch.model.ClassEntity
import scala.tools.apiSearch.model.TermEntity
import scala.tools.apiSearch.settings.Settings
import scala.tools.apiSearch.utils.using

import org.apache.lucene.store.Directory
import org.apache.lucene.store.RAMDirectory

trait IndexUtils extends ExtractionUtils {

  val settings = Settings.fromApplicationConf

  def withDir(f: Directory => Unit) =
    using(new RAMDirectory)(f)

  def withTermIndex(f: TermsIndex => Unit): Unit =
    withDir { dir =>
      val index = new TermsIndex(dir, settings)
      f(index)
    }

  def withTermIndex(sources: String*)(f: TermsIndex => Unit): Unit =
    withTermIndex { index =>
      val entities = sources.toStream.flatMap(extractAll).collect { case t: TermEntity => t }
      index.addEntities(entities).get
      f(index)
    }

  def withClassIndex(f: ClassIndex => Unit): Unit =
    withDir { dir =>
      val index = new ClassIndex(dir)
      f(index)
    }

  def withClassIndex(sources: String*)(f: ClassIndex => Unit): Unit =
    withClassIndex { index =>
      val entities = sources.flatMap(extractAll).collect { case t: ClassEntity => t }
      index.addEntities(entities).get
      f(index)
    }
}

package scaps.searchEngine.index

import scaps.featureExtraction.ExtractionUtils
import scaps.webapi.ClassEntity
import scaps.webapi.TermEntity
import scaps.settings.Settings
import scaps.utils.using

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
    }.get

  def withTermIndex(sources: String*)(f: TermsIndex => Unit): Unit =
    withTermIndex { index =>
      val entities = sources.toStream.flatMap(extractAll).collect { case t: TermEntity => t }
      index.addEntities(entities).get
      f(index)
    }

  def withClassIndex(f: ClassIndex => Unit): Unit =
    withDir { dir =>
      val index = new ClassIndex(dir, settings)
      f(index)
    }.get

  def withClassIndex(sources: String*)(f: ClassIndex => Unit): Unit =
    withClassIndex { index =>
      val entities = sources.flatMap(extractAll).collect { case t: ClassEntity => t }
      index.addEntities(entities).get
      f(index)
    }

  def withModuleIndex(f: ModuleIndex => Unit): Unit =
    withDir { dir =>
      val index = new ModuleIndex(dir)
      f(index)
    }.get
}

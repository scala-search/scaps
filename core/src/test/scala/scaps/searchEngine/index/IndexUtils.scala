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

  def withDir[T](f: Directory => T): T =
    using(new RAMDirectory)(f).get

  def withTermIndex[T](f: TermsIndex => T): T =
    withDir { dir =>
      val index = new TermsIndex(dir, settings)
      f(index)
    }

  def withTermIndex[T](sources: String*)(f: TermsIndex => T): T =
    withTermIndex { index =>
      val entities = sources.toStream.flatMap(extractAll).collect { case t: TermEntity => t }
      index.addEntities(entities).get
      f(index)
    }

  def withClassIndex[T](f: ClassIndex => T): T =
    withDir { dir =>
      val index = new ClassIndex(dir, settings)
      f(index)
    }

  def withClassIndex[T](sources: String*)(f: ClassIndex => T): T =
    withClassIndex { index =>
      val entities = sources.flatMap(extractAll).collect { case t: ClassEntity => t }
      index.addEntities(entities).get
      f(index)
    }

  def withModuleIndex[T](f: ModuleIndex => T): T =
    withDir { dir =>
      val index = new ModuleIndex(dir)
      f(index)
    }

  def withViewIndex[T](f: ViewIndex => T): T =
    withDir { dir =>
      f(new ViewIndex(dir))
    }
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine.index

import scaps.scala.featureExtraction.ExtractionUtils
import scaps.api.TypeDef
import scaps.api.ValueDef
import scaps.settings.Settings
import scaps.utils.using
import org.apache.lucene.store.Directory
import org.apache.lucene.store.RAMDirectory
import scaps.api.Module

trait IndexUtils extends ExtractionUtils {

  val settings = Settings.fromApplicationConf

  def withDir[T](f: Directory => T): T =
    using(new RAMDirectory)(f).get

  def withValueIndex[T](f: ValueIndex => T): T =
    withDir { dir =>
      val index = new ValueIndex(dir, settings)
      f(index)
    }

  def withValueIndex[T](sources: String*)(f: ValueIndex => T): T =
    withValueIndex { index =>
      val entities = sources.toStream.flatMap(extractAll(_)).collect { case t: ValueDef => t }
      index.addEntities(entities).get
      f(index)
    }

  def withTypeIndex[T](f: TypeIndex => T): T =
    withDir { dir =>
      val index = new TypeIndex(dir, settings)
      f(index)
    }

  def withTypeIndex[T](sources: String*)(f: TypeIndex => T): T =
    withTypeIndex { index =>
      val entities = sources.flatMap(extractAll(_)).collect { case t: TypeDef => t }
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

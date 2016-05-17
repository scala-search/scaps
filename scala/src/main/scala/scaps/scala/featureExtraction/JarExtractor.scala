/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.scala.featureExtraction

import java.io.File
import java.util.jar.JarFile
import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.reflect.internal.util.BatchSourceFile
import scaps.api.Definition
import scaps.api.ValueDef
import scaps.api.FileSource
import scala.tools.nsc.doc.ScaladocGlobal
import scala.io.Codec
import scalaz._

class JarExtractor(val compiler: ScaladocGlobal) {
  val scalaExtractor = new ScalaSourceExtractor(compiler)

  def apply(file: File): Stream[ExtractionError \/ Definition] = {
    val jar = new JarFile(file)

    val files = jar.entries().toList.flatMap { entry =>
      if (!entry.isDirectory && entry.getName.endsWith(".scala")) {
        val source = scala.io.Source.fromInputStream(jar.getInputStream(entry))(Codec.UTF8).toSeq
        Some(new BatchSourceFile(entry.getName, source))
      } else {
        None
      }
    }

    scalaExtractor(files).map {
      case \/-(v: ValueDef) =>
        \/-(v.copy(source = FileSource(file.getPath, v.source)))
      case r => r
    }
  }
}

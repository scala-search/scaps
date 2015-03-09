package scala.tools.apiSearch.cli

import scala.tools.apiSearch.utils.CompilerAccess
import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.io.Source
import java.io.FileOutputStream
import java.io.StringWriter
import java.io.FileWriter
import rx.lang.scala.Observable

object ExportEntitiesToCSV extends App with CompilerAccess {
  val extractor = new JarExtractor(compiler)
  val path = args(0)
  val target = args(1)

  val (templateEntities, termEntities) = extractor(path)

  val writer = new FileWriter(target)

  writer.write("Idx; Name; Type Parameters; Type;")

  termEntities.zipWithIndex.subscribe({
    case (entity, idx) =>
      val entry = s"$idx; ${entity.name}; ${entity.typeParameters.mkString(", ")}; ${entity.tpe};\n"
      println(entry)
      writer.write(entry)
  }, println)

  writer.close()
}

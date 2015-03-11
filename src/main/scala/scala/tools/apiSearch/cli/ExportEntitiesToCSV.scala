package scala.tools.apiSearch.cli

import scala.tools.apiSearch.model._
import scala.tools.apiSearch.utils.CompilerAccess
import scala.tools.apiSearch.utils.using
import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.io.Source
import java.io.FileOutputStream
import java.io.StringWriter
import java.io.FileWriter
import java.io.File

object ExportEntitiesToCSV extends App with CompilerAccess {
  val extractor = new JarExtractor(compiler)
  val path = args(0)
  val target = args(1)

  val termsPath = target + ".terms.csv"
  val classesPath = target + ".classes.csv"

  val entities = extractor(new File(path))

  using(new FileWriter(classesPath)) { classesWriter =>
    using(new FileWriter(termsPath)) { termsWriter =>
      classesWriter.write("Idx; Name; Type Parameters; Base Types;\n")
      termsWriter.write("Idx; Name; Fingerprint; Type Parameters; Type;\n")

      entities
        .zipWithIndex
        .foreach {
          case (entity: ClassEntity, idx) =>
            val entry = s"$idx; ${entity.name}; ${entity.typeParameters.mkString(", ")}; ${entity.baseTypes.mkString(", ")};\n"
            println(entry)
            classesWriter.write(entry)
          case (entity: TermEntity, idx) =>
            val entry = s"$idx; ${entity.name}; ${entity.fingerprint}; ${entity.typeParameters.mkString(", ")}; ${entity.tpe};\n"
            println(entry)
            termsWriter.write(entry)
        }
    }
  }

  print("""
    Summary
    =========
""")

  val termEntities = entities.collect { case e: TermEntity => e }
  val byType = termEntities.groupBy(_.tpe)
  val byFingerprint = termEntities.groupBy(_.fingerprint)

  println(s"No. Types: ${byType.size}")
  println(s"No. Fingerprints: ${byFingerprint.size}")
}

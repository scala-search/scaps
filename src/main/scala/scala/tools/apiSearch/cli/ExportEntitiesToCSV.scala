package scala.tools.apiSearch.cli

import scala.tools.apiSearch.model._
import scala.tools.apiSearch.utils.CompilerAccess
import scala.tools.apiSearch.featureExtraction.JarExtractor
import scala.io.Source
import java.io.FileOutputStream
import java.io.StringWriter
import java.io.FileWriter
import rx.lang.scala.Observable
import rx.lang.scala.Scheduler
import rx.lang.scala.schedulers.NewThreadScheduler

object ExportEntitiesToCSV extends App with CompilerAccess {
  val extractor = new JarExtractor(compiler)
  val path = args(0)
  val target = args(1)

  val termsPath = target + ".terms.csv"
  val classesPath = target + ".classes.csv"

  val entities = extractor(path)

  val classesWriter = new FileWriter(classesPath)
  val termsWriter = new FileWriter(termsPath)

  classesWriter.write("Idx; Name; Type Parameters; Base Types;")
  termsWriter.write("Idx; Name; Type Parameters; Type;")

  val o1 = entities
    .zipWithIndex
    .subscribe({
      case (entity: ClassEntity, idx) =>
        val entry = s"$idx; ${entity.name}; ${entity.typeParameters.mkString(", ")}; ${entity.baseTypes.mkString(", ")};\n"
        println(entry)
        classesWriter.write(entry)
      case (entity: TermEntity, idx) =>
        val entry = s"$idx; ${entity.name}; ${entity.typeParameters.mkString(", ")}; ${entity.tpe};\n"
        println(entry)
        termsWriter.write(entry)
    }, println)

  classesWriter.close()
  termsWriter.close()
}

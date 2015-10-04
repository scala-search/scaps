package scaps.scala

import java.io.File
import scaps.featureExtraction.CompilerUtils
import scaps.featureExtraction.JarExtractor
import com.typesafe.config.ConfigFactory
import scaps.api.ScapsControlApi

object Main extends App {
  val settings = ExtractionSettings.fromApplicationConf

  val compiler = CompilerUtils.createCompiler(settings.classpath)

  val extractor = new JarExtractor(compiler)

  settings.modules.foreach { m =>
    println(m)
  }

  val client = new DispatchClient(settings.controlHost, ScapsControlApi.apiPath)[ScapsControlApi]
}

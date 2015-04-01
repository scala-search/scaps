package scala.tools.apiSearch.utils

import scala.tools.nsc.doc.ScaladocGlobalTrait
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter

/**
 * Provides an instance of the Scala presentation compiler
 */
trait CompilerAccess {
  def initCompiler(classpaths: List[String] = Nil) = {
    val settings = new scala.tools.nsc.Settings(msg => throw sys.error(msg))

    // in order to run the tests from sbt, we must add the scala library to the class path
    // but the protection domain might return null when run from eclipse
    val scalaLibClassPath = Option(Class.forName("scala.Unit").getProtectionDomain.getCodeSource)
      .map(_.getLocation.toExternalForm())

    (scalaLibClassPath.toList ::: classpaths).foreach { cp =>
      settings.classpath.append(cp)
      settings.bootclasspath.append(cp)
    }

    println(settings.classpath)

    val reporter = new ConsoleReporter(settings)

    val compiler = new scala.tools.nsc.interactive.Global(settings, reporter) with ScaladocGlobalTrait /* tells compiler to keep doc comment internally */

    compiler.ask(() => new compiler.Run)

    compiler
  }
}

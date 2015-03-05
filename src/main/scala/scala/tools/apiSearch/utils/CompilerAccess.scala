package scala.tools.apiSearch.utils

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.doc.ScaladocGlobalTrait
import scala.tools.nsc.doc.base.MemberLookupBase
import scala.tools.nsc.doc.base.CommentFactoryBase
import scala.tools.nsc.doc.base.LinkTo
import scala.util.Try

/**
 * Provides an instance of the Scala presentation compiler
 */
trait CompilerAccess {
  lazy val compiler: Global = {
    val settings = new scala.tools.nsc.Settings(msg => throw sys.error(msg))

    // in order to run the tests from sbt, we must add the scala library to the class path
    // but the protection domain might return null when run from eclipse
    Option(Class.forName("scala.Unit").getProtectionDomain.getCodeSource)
      .map(_.getLocation.toExternalForm())
      .foreach { scalaLibraryPath =>
        settings.classpath.append(scalaLibraryPath)
        settings.bootclasspath.append(scalaLibraryPath)
      }

    val reporter = new ConsoleReporter(settings)

    val compiler = new scala.tools.nsc.interactive.Global(settings, reporter) with ScaladocGlobalTrait /* tells compiler to keep doc comment internally */

    compiler.ask(() => new compiler.Run)

    compiler
  }
}
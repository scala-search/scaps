import sbt._
import Keys._

object JarExtractorTests {
  lazy val createJar = Def.task {
    val packageName = "jarExtractorTests"
    val jar = (resourceManaged in Test).value / (packageName + ".jar")
    val packageDir = (resourceDirectory in Test).value / packageName

    streams.value.log.info(s"compress $packageDir into $jar")

    IO.delete(jar)
    def entries(f: File): List[File] =
      f :: (if (f.isDirectory) IO.listFiles(f).toList.flatMap(entries(_)) else Nil)

    val files = entries(packageDir).map(d => (d, d.getAbsolutePath.substring(packageDir.getParent.length +1)))

    IO.zip(files, jar)

    Seq(jar)
  }
}
name := "scala-api-search"

scalaVersion := "2.11.5"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

resourceGenerators in Test <+=
  (resourceManaged in Test, resourceDirectory in Test, streams) map { (dir, resourcesDir, streams) =>
  	val packageName = "jarExtractorTests"
  	val jar = dir / (packageName + ".jar")
  	val packageDir = resourcesDir / packageName
    val logger = streams.log
    logger.info(jar.toString)
    logger.info(packageDir.toString)
    IO.delete(jar)
    def entries(f: File):List[File] = f :: (if (f.isDirectory) IO.listFiles(f).toList.flatMap(entries(_)) else Nil)
    IO.zip(entries(packageDir).map(d => (d, d.getAbsolutePath.substring(packageDir.getParent.length +1))), jar)
    Seq[java.io.File](jar)
  }
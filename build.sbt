name := "scala-api-search"

scalaVersion := "2.11.5"

libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.9.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "org.apache.lucene" % "lucene-core" % "4.10.4"

libraryDependencies += "org.apache.lucene" % "lucene-analyzers-common" % "4.10.4"

resourceGenerators in Test <+=
  (resourceManaged in Test, resourceDirectory in Test, streams) map { (dir, resourcesDir, streams) =>
  	val packageName = "jarExtractorTests"
  	val jar = dir / (packageName + ".jar")
  	val packageDir = resourcesDir / packageName
    streams.log.info(s"compress $packageDir into $jar")
    IO.delete(jar)
    def entries(f: File):List[File] = f :: (if (f.isDirectory) IO.listFiles(f).toList.flatMap(entries(_)) else Nil)
    IO.zip(entries(packageDir).map(d => (d, d.getAbsolutePath.substring(packageDir.getParent.length +1))), jar)
    Seq(jar)
  }
  
EclipseKeys.withSource := true

scalacOptions ++= Seq("-feature")
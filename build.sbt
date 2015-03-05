lazy val root = (project in file(".")).
  settings(
    name := "scala-api-search",
    scalaVersion := "2.11.5",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.5",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"
  )

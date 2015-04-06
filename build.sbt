import Dependencies._

lazy val core = (project in file("core"))
  .settings(Commons.settings: _*)
  .settings(libraryDependencies ++= coreDependencies)
  .settings(resourceGenerators in Test <+= JarExtractorTests.createJar)

lazy val benchmark = (project in file("benchmark"))
  .dependsOn(core)
  .settings(Commons.settings: _*)
  .settings(libraryDependencies ++= evaluationDependencies)
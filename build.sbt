lazy val core = (project in file("core"))
  .settings(Commons.settings: _*)
  .settings(libraryDependencies ++= Dependencies.coreDependencies)
  .settings(resourceGenerators in Test <+= JarExtractorTests.createJar)

lazy val evaluation = (project in file("evaluation"))
  .dependsOn(core)
  .settings(Commons.settings: _*)
  .settings(libraryDependencies ++= Dependencies.evaluationDependencies)
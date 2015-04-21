lazy val commonSettings = Seq(
    organization := "ch.hsr",
    scalaVersion := Commons.targetedScalaVersion,
    version := Commons.appVersion,
    resolvers += Opts.resolver.mavenLocalFile,
    scalacOptions ++= Seq(
      "-encoding", "UTF8",
      "-Xlint",
      "-feature",
      "-deprecation",
      "-Ywarn-value-discard",
      "-Xfatal-warnings"))

lazy val root = (project in file("."))
  .aggregate(core, evaluation, sbtPlug)
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false)

lazy val core = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-core",
    libraryDependencies ++= Dependencies.coreDependencies,
    resourceGenerators in Test <+= JarExtractorTests.createJar)

lazy val evaluation = (project in file("evaluation"))
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-evaluation",
    libraryDependencies ++= Dependencies.evaluationDependencies)

lazy val sbtPlug = (project in file("sbtPlugin"))
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-sbt",
    scalaVersion := "2.10.5",
    sbtPlugin := true,
    libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.11.2")

lazy val webservice = (project in file("webservice"))
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-webservice",
    libraryDependencies ++= Dependencies.webserviceDependencies)
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
  .aggregate(core, evaluation, sbtPlug, webservice, webapi_2_10, webapi_2_11)
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
  .dependsOn(webapi_2_10)
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-sbt",
    scalaVersion := Commons.sbtPluginScalaVersion,
    sbtPlugin := true,
    libraryDependencies ++= Seq(
      "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
      "com.lihaoyi" %% "autowire" % "0.2.5",
      "com.lihaoyi" %% "upickle" % "0.2.6"))

lazy val webservice = (project in file("webservice"))
  .dependsOn(webapi_2_11, core)
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-webservice",
    libraryDependencies ++= Dependencies.webserviceDependencies)

def webapiSettings = 
  commonSettings ++ Seq(
    name := "api-search-webapi",
    libraryDependencies ++= Dependencies.webapiDependencies,
    target := baseDirectory.value / s"target-${scalaVersion.value}")

lazy val webapi_2_10 = (project in file("webapi"))
  .settings(webapiSettings: _*)
  .settings(
    scalaVersion := Commons.sbtPluginScalaVersion)

lazy val webapi_2_11 = (project in file("webapi"))
  .settings(webapiSettings: _*)
  .settings(
    scalaVersion := Commons.targetedScalaVersion)
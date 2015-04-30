lazy val utestFramework = new TestFramework("utest.runner.Framework")

lazy val commonSettings = Seq(
    organization := "ch.hsr",
    scalaVersion := Commons.targetedScalaVersion,
    version := Commons.appVersion,
    resolvers += Opts.resolver.mavenLocalFile,
    testFrameworks += utestFramework,
    scalacOptions ++= Seq(
      "-encoding", "UTF8",
      "-Xlint",
      "-feature",
      "-deprecation",
      "-Ywarn-value-discard",
      "-Xfatal-warnings"))

lazy val root = (project in file("."))
  .aggregate(webapi_2_10, webapi_2_11, webapiJS, core, evaluation, webservice, webserviceUI, sbtPlug)
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false)

def webapiSettings = 
  commonSettings ++ Seq(
    name := "api-search-webapi",
    libraryDependencies ++= Dependencies.webapiDependencies,
    target := baseDirectory.value / s"target-${scalaVersion.value}")

lazy val webapi_2_10_cross = (crossProject in file("webapi"))
  .settings(webapiSettings: _*)
  .settings(
    scalaVersion := Commons.sbtPluginScalaVersion,
    // do not run tests for 2.10 because some test dependencies wont resolve
    test := {})

lazy val webapi_2_11_cross = (crossProject in file("webapi"))
  .settings(webapiSettings: _*)
  .settings(
    scalaVersion := Commons.targetedScalaVersion,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "utest" % Dependencies.utestVersion % "test"))

lazy val webapi_2_10 = webapi_2_10_cross.jvm
lazy val webapi_2_11 = webapi_2_11_cross.jvm
lazy val webapiJS = webapi_2_11_cross.js

lazy val core = (project in file("core"))
  .dependsOn(webapi_2_11)
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
    publishArtifact := false,
    libraryDependencies ++= Dependencies.evaluationDependencies)

lazy val webserviceShared_cross = (crossProject in file("webserviceShared"))
  .dependsOn(webapi_2_11_cross)
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-webservice-shared",
    publishArtifact := false,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % Dependencies.scalatagsVersion))

lazy val webserviceShared_JVM = webserviceShared_cross.jvm
lazy val webserviceShared_JS = webserviceShared_cross.js

lazy val webservice = (project in file("webservice"))
  .dependsOn(webserviceShared_JVM, core)
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-webservice",
    publishArtifact := false,
    libraryDependencies ++= Dependencies.webserviceDependencies,
    (resources in Compile) += (fastOptJS in (webserviceUI, Compile)).value.data)

lazy val webserviceUI = (project in file("webserviceUI"))
  .dependsOn(webserviceShared_JS)
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-webservice-ui",
    publishArtifact := false,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % Dependencies.upickleVersion,
      "com.lihaoyi" %%% "autowire" % Dependencies.autowireVersion,
      "com.lihaoyi" %%% "scalatags" % Dependencies.scalatagsVersion,
      "org.scala-js" %%% "scalajs-dom" % "0.8.0"))

lazy val sbtPlug = (project in file("sbtPlugin"))
  .dependsOn(webapi_2_10)
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-sbt",
    scalaVersion := Commons.sbtPluginScalaVersion,
    sbtPlugin := true,
    libraryDependencies ++= Dependencies.sbtPluginDependencies)
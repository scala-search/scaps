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
  .aggregate(core, evaluation, sbtPlug, webserviceJVM, webserviceJS, webapi_2_10, webapi_2_11, webapiJS)
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
    libraryDependencies ++= Dependencies.sbtPluginDependencies)

lazy val webservice = (crossProject in file("webservice"))
  .dependsOn(webapi_2_11_cross)
  .jvmConfigure(_.dependsOn(core))
  .settings(commonSettings: _*)
  .settings(
    name := "api-search-webservice")
  .jvmSettings(
    libraryDependencies ++= Dependencies.webserviceDependencies)
  .jsSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % Dependencies.upickleVersion,
      "com.lihaoyi" %%% "autowire" % Dependencies.autowireVersion,
      "com.lihaoyi" %%% "scalatags" % Dependencies.scalatagsVersion,
      "org.scala-js" %%% "scalajs-dom" % "0.8.0"))

lazy val webserviceJVM = webservice.jvm
  .settings((resources in Compile) += (fastOptJS in (webserviceJS, Compile)).value.data)
lazy val webserviceJS = webservice.js

def webapiSettings = 
  commonSettings ++ Seq(
    name := "api-search-webapi",
    libraryDependencies ++= Dependencies.webapiDependencies,
    target := baseDirectory.value / s"target-${scalaVersion.value}")

lazy val webapi_2_10_cross = (crossProject in file("webapi"))
  .settings(webapiSettings: _*)
  .settings(
    scalaVersion := Commons.sbtPluginScalaVersion)

lazy val webapi_2_11_cross = (crossProject in file("webapi"))
  .settings(webapiSettings: _*)
  .settings(
    scalaVersion := Commons.targetedScalaVersion)

lazy val webapi_2_10 = webapi_2_10_cross.jvm
lazy val webapi_2_11 = webapi_2_11_cross.jvm
lazy val webapiJS = webapi_2_11_cross.js